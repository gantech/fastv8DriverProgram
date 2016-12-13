MODULE OpenFOAM

! This is a pseudo module used to couple FAST v8 with OpenFOAM; it is considered part of the FAST glue code
   USE FAST_Types

   IMPLICIT NONE

   PRIVATE

   TYPE(ProgDesc), PARAMETER            :: OpFM_Ver = ProgDesc( 'OpenFOAM Integration', 'v1.00.00a-bjj', '11-Aug-2015' )


! ==================================================================================================="


      ! ..... Public Subroutines ...................................................................................................

   PUBLIC :: Init_OpFM                           ! Initialization routine
   PUBLIC :: OpFM_CreateActForceNodesMesh        ! Routine to create the mesh containing the actuator force nodes
   PUBLIC :: OpFM_SetInputs                      ! Glue-code routine to update inputs for OpenFOAM
   PUBLIC :: OpFM_SetWriteOutput


CONTAINS
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Init_OpFM( InitInp, p_FAST, AirDens, u_AD14, u_AD, InitInp_AD, y_AD, y_ED, OpFM, InitOut, ErrStat, ErrMsg )
!..................................................................................................................................

   TYPE(OpFM_InitInputType),        INTENT(IN   )  :: InitInp     ! Input data for initialization routine
   TYPE(FAST_ParameterType),        INTENT(IN   )  :: p_FAST      ! Parameters for the glue code
   TYPE(AD14_InputType),            INTENT(IN   )  :: u_AD14      ! AeroDyn14 input data
   TYPE(AD_InputType),              INTENT(INOUT)  :: u_AD        ! AeroDyn input data
   TYPE(AD_InitInputType),          INTENT(IN   )  :: InitInp_AD  ! Input data for initialization routine of AeroDyn
   TYPE(AD_OutputType),             INTENT(IN   )  :: y_AD        ! AeroDyn output data (for mesh mapping)
   TYPE(ED_OutputType),             INTENT(IN)     :: y_ED        ! The outputs of the structural dynamics module
   TYPE(OpenFOAM_Data),             INTENT(INOUT)  :: OpFM        ! data for the OpenFOAM integration module
   TYPE(OpFM_InitOutputType),       INTENT(INOUT)  :: InitOut     ! Output for initialization routine
   INTEGER(IntKi),                  INTENT(  OUT)  :: ErrStat     ! Error status of the operation
   CHARACTER(*),                    INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

      ! local variables
   INTEGER(IntKi)                                   :: k          ! blade loop counter
   INTEGER(IntKi)                                   :: j          ! node counter

   INTEGER(IntKi)                                   :: ErrStat2    ! temporary Error status of the operation
   CHARACTER(ErrMsgLen)                             :: ErrMsg2     ! temporary Error message if ErrStat /= ErrID_None

   CHARACTER(*),   PARAMETER                        :: RoutineName = 'Init_OpFM'

      ! Initialize variables

   ErrStat = ErrID_None
   ErrMsg  = ""

      !............................................................................................
      ! Define parameters here:
      !............................................................................................

      ! number of velocity nodes in the interface:

   OpFM%p%NnodesVel = 1  ! always want the hub point
   IF ( p_FAST%CompAero  == Module_AD14 ) THEN ! AeroDyn 14 needs these velocities
      OpFM%p%NumBl    = SIZE(u_AD14%InputMarkers,1)

      OpFM%p%NnodesVel = OpFM%p%NnodesVel + u_AD14%Twr_InputMarkers%NNodes          ! tower nodes (if any)
      OpFM%p%NnodesVel = OpFM%p%NnodesVel + OpFM%p%NumBl * u_AD14%InputMarkers(1)%Nnodes   ! blade nodes
   ELSEIF ( p_FAST%CompAero  == Module_AD ) THEN ! AeroDyn 15 needs these velocities
      OpFM%p%NumBl = SIZE( u_AD%BladeMotion, 1 )

      OpFM%p%NnodesVel = OpFM%p%NnodesVel + u_AD%TowerMotion%NNodes                 ! tower nodes (if any)
      DO k=1,OpFM%p%NumBl
         OpFM%p%NnodesVel = OpFM%p%NnodesVel + u_AD%BladeMotion(k)%NNodes           ! blade nodes
      END DO
   END IF

      ! number of force nodes in the interface
   Opfm%p%NnodesForceBlade =  InitInp%NumActForcePtsBlade 
   OpFM%p%NnodesForceTower = InitInp%NumActForcePtsTower
   OpFM%p%NnodesForce = 1 + OpFM%p%NumBl * InitInp%NumActForcePtsBlade + InitInp%NumActForcePtsTower

   IF ( p_FAST%CompAero == Module_AD ) THEN ! AeroDyn 15 needs mapping of line2 meshes to point meshes
      if ( y_AD%TowerLoad%NNodes > 0 ) then
         OpFM%p%NMappings = OpFM%p%NumBl + 1
      else
         OpFM%p%NMappings = OpFM%p%NumBl
      end if
   END IF
   
   ! initialize the arrays:
   call OpFM_CreateActForceMotionsMesh( InitInp, p_FAST, u_AD14, u_AD, y_ED, y_AD, OpFM, ErrStat2, ErrMsg2)
   call SetOpFMPositions(p_FAST, u_AD14, u_AD, y_ED, OpFM)

      !............................................................................................
   ! Allocate arrays and set up mappings to point loads (for AD15 only):
      ! (bjj: note that normally I'd put these things in the FAST_ModuleMapType, but I don't want
      ! to add OpenFOAM integrations in the rest fo the code).
      !............................................................................................
   IF ( p_FAST%CompAero == Module_AD ) THEN ! AeroDyn 15 needs mapping of line2 meshes to point meshes

      ! Allocate space for mapping data structures
      ALLOCATE( OpFM%m%Line2_to_Point_Loads(OpFM%p%NMappings), OpFM%m%Line2_to_Point_Motions(OpFM%p%NMappings),STAT=ErrStat2)

      do k=1,OpFM%p%NMappings
         call MeshCopy (  SrcMesh  = OpFM%m%ActForceLoads(k)   &
                        , DestMesh = OpFM%m%ActForceMotions(k) &
                        , CtrlCode = MESH_SIBLING          &
                        , IOS      = COMPONENT_OUTPUT      &
                        , Orientation     = .true.         &
                        , TranslationDisp = .true.         &
                        , TranslationVel  = .true.         &
                        , RotationVel     = .true.         &
                        , ErrStat  = ErrStat2              &
                        , ErrMess  = ErrMsg2               )
      end do

      ! create the mapping data structures:
      DO k=1,OpFM%p%NumBl
         IF (p_FAST%CompElast == Module_ED ) THEN
            call MeshMapCreate( y_ED%BladeLn2Mesh(k), OpFM%m%ActForceMotions(k), OpFM%m%Line2_to_Point_Motions(k),  ErrStat2, ErrMsg2 );
         ELSEIF (p_FAST%CompElast == Module_BD ) THEN
            call MeshMapCreate( BD%y(k)%BldMotion, OpFM%m%ActForceMotions(k), OpFM%m%Line2_to_Point_Motions(k),  ErrStat2, ErrMsg2 );
         END IF
         call MeshMapCreate( y_AD%BladeLoad(k), OpFM%m%ActForceLoads(k), OpFM%m%Line2_to_Point_Loads(k),  ErrStat2, ErrMsg2 );
      END DO

      do k=OpFM%p%NumBl+1,OpFM%p%NMappings
         call MeshMapCreate( y_ED%TowerLn2Mesh, OpFM%m%ActForceMotions(k), OpFM%m%Line2_to_Point_Motions(k),  ErrStat2, ErrMsg2 );

         if ( y_AD%TowerLoad%nnodes > 0 ) then ! we can have an input mesh on the tower without having an output mesh.
            call MeshMapCreate( y_AD%TowerLoad, OpFM%m%ActForceLoads(k), OpFM%m%Line2_to_Point_Loads(k),  ErrStat2, ErrMsg2 );
         end if

      end do

   END IF

   RETURN

END SUBROUTINE Init_OpFM
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE OpFM_SetInputs( p_FAST, p_AD14, u_AD14, y_AD14, u_AD, y_AD, y_ED, y_SrvD, OpFM, ErrStat, ErrMsg )
!..................................................................................................................................

   TYPE(FAST_ParameterType),       INTENT(IN    )  :: p_FAST      ! Parameters for the glue code
   TYPE(AD14_ParameterType),       INTENT(IN)      :: p_AD14      ! The parameters from AeroDyn14 (for mesh transfer with improperly set meshes)
   TYPE(AD14_InputType),           INTENT(IN)      :: u_AD14      ! The input meshes (already calculated) from AeroDyn14
   TYPE(AD14_OutputType),          INTENT(IN)      :: y_AD14      ! The output meshes (already calculated) from AeroDyn14
   TYPE(AD_InputType),             INTENT(IN)      :: u_AD        ! The input meshes (already calculated) from AeroDyn
   TYPE(AD_OutputType),            INTENT(IN)      :: y_AD        ! The output meshes (already calculated) from AeroDyn
   TYPE(ED_OutputType),            INTENT(IN)      :: y_ED        ! The outputs of the structural dynamics module
   TYPE(SrvD_OutputType),          INTENT(IN)      :: y_SrvD      ! The outputs of the ServoDyn module (control)
   TYPE(OpenFOAM_Data),            INTENT(INOUT)   :: OpFM        ! data for the OpenFOAM integration module
   INTEGER(IntKi),                 INTENT(  OUT)   :: ErrStat     ! Error status of the operation
   CHARACTER(*),                   INTENT(  OUT)   :: ErrMsg      ! Error message if ErrStat /= ErrID_None

   ! local variables
   INTEGER(IntKi)                                  :: ErrStat2    ! temporary Error status of the operation
   CHARACTER(ErrMsgLen)                            :: ErrMsg2     ! temporary Error message if ErrStat /= ErrID_None

   CHARACTER(*),   PARAMETER                       :: RoutineName = 'OpFM_SetInputs'


   ErrStat = ErrID_None
   ErrMsg  = ""

      ! set the positions
   call SetOpFMPositions(p_FAST, u_AD14, u_AD, y_ED, OpFM)

      ! set the forces
   call SetOpFMForces(p_FAST, p_AD14, u_AD14, y_AD14, u_AD, y_AD, y_ED, OpFM, ErrStat2, ErrMsg2)
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

      ! set SuperController inputs
   if (p_FAST%CompServo == Module_SrvD) then
      if (allocated(y_SrvD%SuperController).and. associated(OpFM%u%SuperController)) OpFM%u%SuperController = y_SrvD%SuperController
   end if


END SUBROUTINE OpFM_SetInputs
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SetOpFMPositions(p_FAST, u_AD14, u_AD, y_ED, OpFM)

   TYPE(OpenFOAM_Data),            INTENT(INOUT)   :: OpFM        ! data for the OpenFOAM integration module
   TYPE(AD14_InputType),           INTENT(IN)      :: u_AD14      ! The input meshes (already calculated) from AeroDyn14
   TYPE(AD_InputType),             INTENT(IN)      :: u_AD        ! The input meshes (already calculated) from AeroDyn
   TYPE(ED_OutputType),            INTENT(IN)      :: y_ED        ! The outputs of the structural dynamics module
   TYPE(FAST_ParameterType),       INTENT(IN   )   :: p_FAST      ! FAST parameter data


      ! Local variables:

   INTEGER(IntKi)                                  :: J           ! Loops through nodes / elements.
   INTEGER(IntKi)                                  :: K           ! Loops through blades.
   INTEGER(IntKi)                                  :: Node        ! Node number for blade/node on mesh



   !-------------------------------------------------------------------------------------------------
   Node = 1   ! undisplaced hub position    ( Maybe we also want to use the displaced position (add y_ED%HubPtMotion%TranslationDisp) at some point in time.)
   OpFM%u%pxVel(Node) = y_ED%HubPtMotion%Position(1,1)
   OpFM%u%pyVel(Node) = y_ED%HubPtMotion%Position(2,1)
   OpFM%u%pzVel(Node) = y_ED%HubPtMotion%Position(3,1)


   IF (p_FAST%CompAero == MODULE_AD14) THEN

         ! blade nodes
      DO K = 1,OpFM%p%NumBl
         DO J = 1,u_AD14%InputMarkers(K)%nnodes  !this mesh isn't properly set up (it's got the global [absolute] position and no reference position)
            Node = Node + 1
            OpFM%u%pxVel(Node) = u_AD14%InputMarkers(K)%Position(1,J)
            OpFM%u%pyVel(Node) = u_AD14%InputMarkers(K)%Position(2,J)
            OpFM%u%pzVel(Node) = u_AD14%InputMarkers(K)%Position(3,J)
         END DO !J = 1,p%BldNodes ! Loop through the blade nodes / elements
      END DO !K = 1,OpFM%p%NumBl

         ! tower nodes
      DO J=1,u_AD14%Twr_InputMarkers%nnodes
         Node = Node + 1
         OpFM%u%pxVel(Node) = u_AD14%Twr_InputMarkers%TranslationDisp(1,J) + u_AD14%Twr_InputMarkers%Position(1,J)
         OpFM%u%pyVel(Node) = u_AD14%Twr_InputMarkers%TranslationDisp(2,J) + u_AD14%Twr_InputMarkers%Position(2,J)
         OpFM%u%pzVel(Node) = u_AD14%Twr_InputMarkers%TranslationDisp(3,J) + u_AD14%Twr_InputMarkers%Position(3,J)
      END DO

   ELSEIF (p_FAST%CompAero == MODULE_AD) THEN

         ! blade nodes
      DO K = 1,OpFM%p%NumBl
         DO J = 1,u_AD%BladeMotion(k)%Nnodes

            Node = Node + 1
            OpFM%u%pxVel(Node) = u_AD%BladeMotion(k)%TranslationDisp(1,j) + u_AD%BladeMotion(k)%Position(1,j)
            OpFM%u%pyVel(Node) = u_AD%BladeMotion(k)%TranslationDisp(2,j) + u_AD%BladeMotion(k)%Position(2,j)
            OpFM%u%pzVel(Node) = u_AD%BladeMotion(k)%TranslationDisp(3,j) + u_AD%BladeMotion(k)%Position(3,j)

         END DO !J = 1,p%BldNodes ! Loop through the blade nodes / elements
      END DO !K = 1,p%OpFM%p%NumBl

         ! tower nodes
      DO J=1,u_AD%TowerMotion%nnodes
         Node = Node + 1
         OpFM%u%pxVel(Node) = u_AD%TowerMotion%TranslationDisp(1,J) + u_AD%TowerMotion%Position(1,J)
         OpFM%u%pyVel(Node) = u_AD%TowerMotion%TranslationDisp(2,J) + u_AD%TowerMotion%Position(2,J)
         OpFM%u%pzVel(Node) = u_AD%TowerMotion%TranslationDisp(3,J) + u_AD%TowerMotion%Position(3,J)
      END DO

   END IF



END SUBROUTINE SetOpFMPositions
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SetOpFMForces(p_FAST, p_AD14, u_AD14, y_AD14, u_AD, y_AD, y_ED, OpFM, ErrStat, ErrMsg)

   TYPE(OpenFOAM_Data),            INTENT(INOUT)   :: OpFM        ! data for the OpenFOAM integration module
   TYPE(AD14_ParameterType),       INTENT(IN)      :: p_AD14      ! The input meshes (already calculated) from AeroDyn14
   TYPE(AD14_InputType),           INTENT(IN)      :: u_AD14      ! The input meshes (already calculated) from AeroDyn14
   TYPE(AD14_OutputType),          INTENT(IN)      :: y_AD14      ! The output meshes (already calculated) from AeroDyn14
   TYPE(AD_InputType),             INTENT(IN)      :: u_AD        ! The input meshes (already calculated) from AeroDyn
   TYPE(AD_OutputType),            INTENT(IN)      :: y_AD        ! The output meshes (already calculated) from AeroDyn
   TYPE(ED_OutputType),            INTENT(IN)      :: y_ED        ! The outputs of the structural dynamics module
   TYPE(FAST_ParameterType),       INTENT(IN   )   :: p_FAST      ! FAST parameter data
   !TYPE(FAST_MiscVarType),         INTENT(IN   )   :: m_FAST      ! misc FAST data, including inputs from external codes like Simulink
   INTEGER(IntKi),                 INTENT(  OUT)   :: ErrStat     ! Error status of the operation
   CHARACTER(*),                   INTENT(  OUT)   :: ErrMsg      ! Error message if ErrStat /= ErrID_None


      ! Local variables:
   REAL(ReKi )                                     :: factor      ! scaling factor to get normalized forces for OpenFOAM

   INTEGER(IntKi)                                  :: J           ! Loops through nodes / elements
   INTEGER(IntKi)                                  :: K           ! Loops through blades.
   INTEGER(IntKi)                                  :: Node        ! Node number for blade/node on mesh
   INTEGER(IntKi)                                  :: ErrStat2    ! temporary Error status of the operation
   CHARACTER(ErrMsgLen)                            :: ErrMsg2     ! temporary Error message if ErrStat /= ErrID_None

   CHARACTER(*),   PARAMETER                       :: RoutineName = 'SetOpFMForces'


   ErrStat = ErrID_None
   ErrMsg  = ''

   !-------------------------------------------------------------------------------------------------
   Node = 1   ! undisplaced hub position  (no aerodynamics computed here)
   OpFM%u%fx(Node) = 0.0_ReKi
   OpFM%u%fy(Node) = 0.0_ReKi
   OpFM%u%fz(Node) = 0.0_ReKi


   IF (p_FAST%CompAero == MODULE_AD14) THEN

      CALL SetErrStat(ErrID_Fatal, 'Error AeroDyn14 is not supported yet with different number of velocity and force actuator nodes', ErrStat, ErrMsg, RoutineName)
      RETURN

   ELSEIF (p_FAST%CompAero == MODULE_AD) THEN

         !.......................
         ! blade nodes
         !.......................

      DO K = 1,OpFM%p%NumBl

         ! mesh mapping from line2 mesh to point mesh

         IF (p_FAST%CompElast == Module_ED ) THEN
            call Transfer_Line2_to_Point( y_ED%BladeLn2Mesh(k), OpFM%m%ActForceMotions(k), OpFM%m%Line2_to_Point_Motions(k), ErrStat2, ErrMsg2 )
         ELSEIF (p_FAST%CompElast == Module_BD ) THEN
            call Transfer_Line2_to_Point( BD%y(k)%BldMotion, OpFM%m%ActForceMotions(k), OpFM%m%Line2_to_Point_Motions(k), ErrStat2, ErrMsg2 )
         END IF

         call Transfer_Line2_to_Point( y_AD%BladeLoad(k), OpFM%m%ActForceLoads(k), OpFM%m%Line2_to_Point_Loads(k), ErrStat2, ErrMsg2, u_AD%BladeMotion(k), OpFM%m%AeroMotions(k) )

         DO J = 1, OpFM%p%NnodesForceBlade
            Node = Node + 1
            OpFM%u%fx(Node) = OpFM%m%AeroLoads(k)%Force(1,j) / OpFM%p%AirDens
            OpFM%u%fy(Node) = OpFM%m%AeroLoads(k)%Force(2,j) / OpFM%p%AirDens
            OpFM%u%fz(Node) = OpFM%m%AeroLoads(k)%Force(3,j) / OpFM%p%AirDens
         END DO !J = 1,p%BldNodes ! Loop through the blade nodes / elements

      END DO !K = 1,OpFM%p%NumBl

         !.......................
         ! tower nodes
         !.......................

            ! mesh mapping from line2 mesh to point mesh
      k = SIZE(u_AD%BladeMotion) + 1

      call Transfer_Line2_to_Point( y_ED%TowerLn2Mesh, OpFM%m%ActForceMotions(k), OpFM%m%Line2_to_Point_Motions(k), ErrStat2, ErrMsg2 )

      call Transfer_Line2_to_Point( y_AD%TowerLoad, OpFM%m%ActForceLoads(k), OpFM%m%Line2_to_Point_Loads(k), ErrStat2, ErrMsg2, u_AD%TowerMotion, OpFM%m%AeroMotions(k) )

      DO J=1,OpFM%p%NnodesForceTower
         Node = Node + 1
         OpFM%u%fx(Node) = OpFM%m%AeroLoads(k)%Force(1,j) / OpFM%p%AirDens
         OpFM%u%fy(Node) = OpFM%m%AeroLoads(k)%Force(2,j) / OpFM%p%AirDens
         OpFM%u%fz(Node) = OpFM%m%AeroLoads(k)%Force(3,j) / OpFM%p%AirDens
      END DO

   END IF

END SUBROUTINE SetOpFMForces
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE OpFM_CreateActForceMotionsMesh( p_FAST, y_ED, OpFM, ErrStat, ErrMsg )
!..................................................................................................................................

   TYPE(FAST_ParameterType),        INTENT(IN   )  :: p_FAST      ! Parameters for the glue code
   TYPE(ED_OutputType),             INTENT(IN)     :: y_ED        ! The outputs of the structural dynamics module
   TYPE(OpenFOAM_Data),             INTENT(INOUT)  :: OpFM        ! data for the OpenFOAM integration module
   INTEGER(IntKi),                  INTENT(  OUT)  :: ErrStat     ! Error status of the operation
   CHARACTER(*),                    INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

      ! local variables
   TYPE(MeshType) , DIMENSION(:), ALLOCATABLE      :: tmpActForceMotionsMesh   !< temporary mesh for interpolating orientation to actuator force points [-]
   INTEGER(IntKi)                                  :: k          ! blade loop counter
   INTEGER(IntKi)                                  :: i,j          ! node counter

   INTEGER(IntKi)                                  :: ErrStat2    ! temporary Error status of the operation
   CHARACTER(ErrMsgLen)                            :: ErrMsg2     ! temporary Error message if ErrStat /= ErrID_None

   CHARACTER(*),   PARAMETER                       :: RoutineName = 'OpFM_CreateActForceMotionsMesh'

      ! Initialize variables

   ErrStat = ErrID_None
   ErrMsg  = ""

   NumBl   = 0


   IF ( p_FAST%CompAero  == Module_AD14 ) THEN 

      CALL SetErrStat(ErrID_Fatal, 'Error AeroDyn14 is not supported yet with different number of velocity and force actuator nodes', ErrStat, ErrMsg, RoutineName)
      RETURN

   ELSEIF ( p_FAST%CompAero == Module_AD ) THEN ! AeroDyn 15 needs mapping of line2 meshes to point meshes
      ! Allocate space for mapping data structures
      ALLOCATE(tmpActForceMotionsMesh(OpFM%p%NMappings) , STAT=ErrStat2)
      IF (ErrStat2 /= 0) THEN
         CALL SetErrStat(ErrID_Fatal, 'Error allocating force nodes mesh mapping types', ErrStat, ErrMsg, RoutineName)
         RETURN
      END IF
      CALL OpFM_CreateTmpActForceMotionsMesh( p_FAST, y_ED, OpFM%p, tmpActForceMotionsMesh, ErrStat2, ErrMsg2 )
           call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
           if (errStat >= AbortErrLev) return

      DO k=1,OpFM%p%NumBl
         call MeshCreate ( BlankMesh = OpFM%m%ActForceMotions(k)         &
                          ,IOS       = COMPONENT_INPUT             &
                          ,Nnodes    = OpFM%p%NnodeForceBlade &
                          ,ErrStat   = ErrStat2                    &
                          ,ErrMess   = ErrMsg2                     &
                          
                         )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
               IF (ErrStat >= AbortErrLev) RETURN

         do j=1,OpFM%p%NnodesForceBlade
            call MeshPositionNode(OpFM%m%ActForceMotions(k), j, tmpActForceMotionsMesh(k)%position(:,j), errStat2, errMsg2, &
                                  orient=tmpActForceMotionsMesh(k)%irentation(:,:,j) )
               call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

            call MeshConstructElement(OpFM%m%ActForceMotions(k), ELEMENT_POINT, errStat2, errMsg2, p1=j )
               call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
         end do !j

         call MeshCommit(OpFM%m%ActForceMotions(k)), errStat2, errMsg2 )
            call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
            if (errStat >= AbortErrLev) return
      END DO

      DO k=OpFM%p%NumBl+1,OpFM%p%NMappings !Tower if present
         call MeshCreate ( BlankMesh = OpFM%m%ActForceMotions(k)         &
                          ,IOS       = COMPONENT_INPUT             &
                          ,Nnodes    = OpFM%p%NnodeForceBlade &
                          ,ErrStat   = ErrStat2                    &
                          ,ErrMess   = ErrMsg2                     &
                          
                         )
               CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
               IF (ErrStat >= AbortErrLev) RETURN

         do j=1,OpFM%p%NnodesForceTower
            call MeshPositionNode(OpFM%m%ActForceMotions(k), j, tmpActForceMotionsMesh(k)%position(:,j), errStat2, errMsg2, &
                                  orient=tmpActForceMotionsMesh(k)%irentation(:,:,j) )
               call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

            call MeshConstructElement(OpFM%m%ActForceMotions(k), ELEMENT_POINT, errStat2, errMsg2, p1=j )
               call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
         end do !j

         call MeshCommit(OpFM%m%ActForceMotions(k)), errStat2, errMsg2 )
            call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
            if (errStat >= AbortErrLev) return
      END DO

   END IF
   

END SUBROUTINE OpFM_CreateActForceMotionsMesh
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE OpFM_CreateTmpActForceMotionsMesh( p_FAST, y_ED, p_OpFM, tmpActForceMotions, ErrStat, ErrMsg )
!..................................................................................................................................

   TYPE(FAST_ParameterType),        INTENT(IN   )  :: p_FAST      ! Parameters for the glue code
   TYPE(ED_OutputType),             INTENT(IN   )  :: y_ED        ! The outputs of the structural dynamics module
   TYPE(OpFM_ParameterType),        INTENT(IN   )  :: OpFM        ! data for the OpenFOAM integration module
   TYPE(MeshType),                  INTENT(INOUT)  :: tmpActForceMotions ! temporary mesh to create the actuator force nodes
   INTEGER(IntKi),                  INTENT(  OUT)  :: ErrStat     ! Error status of the operation
   CHARACTER(*),                    INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

      ! local variables
   TYPE(MeshMapType) , DIMENSION(:), ALLOCATABLE   :: tmp_line2_to_point_Motions    !< mapping data structure to convert orientation of structural nodes to actuator force nodes [-]
   TYPE(MeshType) , DIMENSION(:), ALLOCATABLE      :: tmp_StructModel_Mesh   !< temporary mesh copying Structural model mesh
   REAL(ReKi), DIMENSION(:,:), ALLOCATABLE         :: forceNodePositions  ! new positions for the force actuator nodes
   INTEGER(IntKi)                                  :: NumBl      ! number of blades
   INTEGER(IntKi)                                  :: k          ! blade loop counter
   INTEGER(IntKi)                                  :: i,j          ! node counter

   INTEGER(IntKi)                                  :: ErrStat2    ! temporary Error status of the operation
   CHARACTER(ErrMsgLen)                            :: ErrMsg2     ! temporary Error message if ErrStat /= ErrID_None

   CHARACTER(*),   PARAMETER                       :: RoutineName = 'OpFM_CreateTmpActForceMotionsMesh'

      ! Initialize variables

   ErrStat = ErrID_None
   ErrMsg  = ""

   ! Make a copy of the Structural model mesh with the reference orientation set to zero
   ALLOCATE(tmp_StructModel_Mesh(OpFM%p%NMappings) , STAT=ErrStat2)
   IF (ErrStat2 /= 0) THEN
      CALL SetErrStat(ErrID_Fatal, 'Error allocating temporary copy of ElastoDyn mesh type', ErrStat, ErrMsg, RoutineName)
      RETURN
   END IF
   CALL CreateTmpStructModelMesh(p_FAST, y_ED, p_OpFM, tmp_StructModelMesh, ErrStat2, ErrMsg2 )
   CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   IF (ErrStat >= AbortErrLev) RETURN
 
   ! Allocate space for mapping data structures
   ALLOCATE( tmp_line2_to_point_Motions(OpFM%p%NMappings),STAT=ErrStat2)
   IF (ErrStat2 /= 0) THEN
      CALL SetErrStat(ErrID_Fatal, 'Error allocating temporary actuator force mesh mapping types', ErrStat, ErrMsg, RoutineName)
      RETURN
   END IF

   ! create meshes to map:
   ALLOCATE(forceNodePositions(3,NnodesForceBlade)) ! Allocate space to create new positions
   DO k=1,OpFM%p%NumBl
      call MeshCreate ( BlankMesh = tmpActForceMotions(k)         &
           ,IOS       = COMPONENT_INPUT             &
           ,Nnodes    = InitInp%NumActForcePtsBlade &
           ,ErrStat   = ErrStat2                    &
           ,ErrMess   = ErrMsg2                     &
           ,force     = .false.                     &
           ,moment    = .false.                     &
           ,orientation = .true.                    &
           )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      IF (ErrStat >= AbortErrLev) RETURN
      
      call CalcForceActuatorPositions(p_OpFM%NnodesForceBlade, tmp_StructModelMesh(k)%position, forceNodePositions, errStat2, errMsg2)
      do j=1,p_OpFM%NnodesForceBlade
         call MeshPositionNode(tmpActForceMotions(k), j, forceNodePositions(:,j), errStat2, errMsg2)
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
         
         call MeshConstructElement( tmpActForceMotions(k), ELEMENT_POINT, errStat2, errMsg2, p1=j )
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
      end do !j
      
      call MeshCommit(tmpActForceMotions(k), errStat2, errMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
      if (errStat >= AbortErrLev) return
   end do
   DEALLOCATE(forceNodePositions) ! Free space

   ALLOCATE(forceNodePositions(3,NnodesForceTower)) ! Allocate space to create new positions
   DO k=OpFM%p%NumBl+1,OpFM%p%NMappings   
      call CalcForceActuatorPositions(p_OpFM%NnodesForceTower, tmp_StructModelMesh(k)%position, forceNodePositions, errStat2, errMsg2)

      call MeshCreate ( BlankMesh = tmpActForceMotions(k)        &
           ,IOS       = COMPONENT_INPUT             &
           ,Nnodes    = InitInp%NumActForcePtsTower &
           ,ErrStat   = ErrStat2                    &
           ,ErrMess   = ErrMsg2                     &
           ,force     = .false.                     &
           ,moment    = .false.                     &
           ,orientation = .true.                    &
           )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      IF (ErrStat >= AbortErrLev) RETURN
      
      do j=1,InitInp%NumActForcePtsTower
         call MeshPositionNode(tmpActForceMotions(k), j, forceNodePositions(:,j), errStat2, errMsg2,&
              orient=y_AD%TowerLoad%RefOrientation(:,:,j))
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
         
         call MeshConstructElement( tmpActForceMotions(k), ELEMENT_POINT, errStat2, errMsg2, p1=j )
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
      end do !j
      
      call MeshCommit(tmpActForceMotions(k), errStat2, errMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
   if (errStat >= AbortErrLev) return
   END DO
   DEALLOCATE(forceNodePositions) ! Free space
   
   ! create the mapping data structures:
   DO k=1,OpFM%p%NumBl
      call MeshMapCreate( u_AD%BladeMotion(k), tmpActForceMotions(k), tmp_line2_to_point_Motions(k),  ErrStat2, ErrMsg2 );
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   END DO
   
   DO k=OpFM%p%NumBl+1,OpFM%p%NMappings
      if ( y_AD%TowerLoad%nnodes > 0 ) then ! we can have an input mesh on the tower without having an output mesh.
         call MeshMapCreate( u_AD%TowerMotion, tmpActForceMotions(k), tmp_line2_to_point_Motions(k),  ErrStat2, ErrMsg2 );
         call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      end if
   END DO
   
   ! Map the orientation
   DO K = 1,p_OpFM%NMappings
      ! mesh mapping from line2 mesh to point mesh
      call Transfer_Line2_to_Point( tmp_StructModelMesh(k), tmpActForceMotions(k), velocity_to_force_nodes_Motions(k), ErrStat2, ErrMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      
   END DO
   

   RETURN

END SUBROUTINE OpFM_CreateTmpActForceMotionsMesh
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE CreateTmpStructModelMesh(p_FAST, y_ED, p_OpFM, tmpStructModelMesh, ErrStat, ErrMsg )

  TYPE(FAST_ParameterType),        INTENT(IN   )  :: p_FAST      ! Parameters for the glue code
  TYPE(ED_OutputType),             INTENT(IN   )  :: y_ED        ! The outputs of the structural dynamics module
  TYPE(OpFM_ParameterType),        INTENT(IN   )  :: p_OpFM      ! Parameters of the OpenFOAM integration module
  TYPE(MeshType),                  INTENT(INOUT)  :: tmpStructModelMesh ! temporary copy of structural model mesh
  INTEGER(IntKi),                  INTENT(  OUT)  :: ErrStat     ! Error status of the operation
  CHARACTER(*),                    INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


  !Local variables
  INTEGER(IntKi)                                  :: nNodesStructModel ! Number of nodes (tower/blade) in the structural model mesh


  IF (p_FAST%CompElast == Module_ED ) THEN


     DO K = 1,p_OpFM%NumBl

        nNodesStuctModel = SIZE(y_ED%BladeLn2Mesh(K)%positions(1,:))
         
        CALL MeshCreate( BlankMesh       = tmpStructModelMesh(K)      &
                       , NNodes          = nNodesStructModel          &
                       , IOS             = COMPONENT_OUTPUT       .                 &
                       , Orientation     = .TRUE.                 &
                       , RotationVel     = .TRUE.                 &
                       , TranslationVel  = .TRUE.                 &
                       , RotationAcc     = .TRUE.                 &
                       , TranslationAcc  = .TRUE.                 &
                       , ErrStat         = ErrStat2               &
                       , ErrMess         = ErrMsg2                )
        CALL CheckError( ErrStat2, ErrMsg2 )
        IF (ErrStat >= AbortErrLev) RETURN
        
        DO J = 1,nNodesStructModel
           CALL MeshPositionNode ( tmpStructModelMesh(K), J, y_ED%BladeLn2Mesh(K)%Position(:,J), ErrStat2, ErrMsg2 )
           CALL CheckError( ErrStat2, ErrMsg2 )
           IF (ErrStat >= AbortErrLev) RETURN
        END DO
        
        ! create elements:      
        DO J = 2,nNodesStuctModel-1
           
           CALL MeshConstructElement ( Mesh      = tmpStructModelMesh(K)  &
                                                 , Xelement = ELEMENT_LINE2      &
                                                 , P1       = J-1                &   ! node1 number
                                                 , P2       = J                  &   ! node2 number
                                                 , ErrStat  = ErrStat2           &
                                                 , ErrMess  = ErrMsg2            )
           CALL CheckError( ErrStat2, ErrMsg2 )
           IF (ErrStat >= AbortErrLev) RETURN
           
        END DO ! J (blade nodes)
        
        ! add the other extra element, connecting the first node on the blade:
        CALL MeshConstructElement ( Mesh      = tmpStructModelMesh(K)  &
             , Xelement = ELEMENT_LINE2      &
             , P1       = nNodesStructModel  &   ! node1 number (extra node at root)
             , P2       = 1                  &   ! node2 number (first node on blade)
             , ErrStat  = ErrStat2           &
             , ErrMess  = ErrMsg2            )         
        CALL CheckError( ErrStat2, ErrMsg2 )
        IF (ErrStat >= AbortErrLev) RETURN
        
        
        ! that's our entire mesh:
        CALL MeshCommit ( tmpStructModelMesh(K), ErrStat2, ErrMsg2 )   
        CALL CheckError( ErrStat2, ErrMsg2 )
        IF (ErrStat >= AbortErrLev) RETURN
        
     END DO
     
     DO K = p_OpFM%NumBl+1, p_OpFM%NMappings
        
        nNodesStuctModel = SIZE(y_ED%TowerLn2Mesh(K)%positions(1,:))

        CALL MeshCreate( BlankMesh       = tmpStructModelMesh(K)      &
                       , NNodes          = nNodesStructModel          &
                       , IOS             = COMPONENT_OUTPUT       .                 &
                       , Orientation     = .TRUE.                 &
                       , TranslationAcc  = .TRUE.                 &
                       , ErrStat         = ErrStat2               &
                       , ErrMess         = ErrMsg2                )
        CALL CheckError( ErrStat2, ErrMsg2 )
        IF (ErrStat >= AbortErrLev) RETURN

        DO J = 1,nNodesStructModel
           CALL MeshPositionNode ( tmpStructModelMesh(K), J, y_ED%TowerLn2Mesh(K)%Position(:,J), ErrStat2, ErrMsg2 )
           CALL CheckError( ErrStat2, ErrMsg2 )
           IF (ErrStat >= AbortErrLev) RETURN
        END DO
        
        ! create elements:      
        DO J = 2,nNodesStuctModel-1
           
           CALL MeshConstructElement ( Mesh      = tmpStructModelMesh(K)  &
                                                 , Xelement = ELEMENT_LINE2      &
                                                 , P1       = J-1                &   ! node1 number
                                                 , P2       = J                  &   ! node2 number
                                                 , ErrStat  = ErrStat2           &
                                                 , ErrMess  = ErrMsg2            )
           CALL CheckError( ErrStat2, ErrMsg2 )
           IF (ErrStat >= AbortErrLev) RETURN
           
        END DO ! J (blade nodes)
        
        ! add the other extra element, connecting the first node on the blade:
        CALL MeshConstructElement ( Mesh      = tmpStructModelMesh(K)  &
             , Xelement = ELEMENT_LINE2      &
             , P1       = nNodesStructModel  &   ! node1 number (extra node at root)
             , P2       = 1                  &   ! node2 number (first node on blade)
             , ErrStat  = ErrStat2           &
             , ErrMess  = ErrMsg2            )         
        CALL CheckError( ErrStat2, ErrMsg2 )
        IF (ErrStat >= AbortErrLev) RETURN
        
        
        ! that's our entire mesh:
        CALL MeshCommit ( tmpStructModelMesh(K), ErrStat2, ErrMsg2 )   
        CALL CheckError( ErrStat2, ErrMsg2 )
        IF (ErrStat >= AbortErrLev) RETURN
        
     END DO
     
     
  ELSEIF (p_FAST%CompElast == Module_BD ) THEN

     CALL SetErrStat(ErrID_Fatal, 'Error BeamDyn is not supported yet with OpenFOAM module', ErrStat, ErrMsg, RoutineName)
     RETURN
     
  END IF

  RETURN 
END SUBROUTINE CreateTmpStructModelMesh

SUBROUTINE CalcForceActuatorPositions(numForcePts, velPositions, forceNodePositions, ErrStat2, ErrMsg2)

  INTEGER(IntKi),         INTENT(IN   )  :: numForcePts              ! number of force actuator nodes desired
  REAL(ReKi),   POINTER                  :: velPositions(:,:)        ! AeroDyn output data (for mesh mapping)
  REAL(ReKi),             INTENT(INOUT)  :: forceNodePositions(:,:)  ! Array to store the newly created positions

  INTEGER(IntKi)                         :: ErrStat2    ! temporary Error status of the operation
  CHARACTER(ErrMsgLen)                   :: ErrMsg2     ! temporary Error message if ErrStat /= ErrID_None


  INTEGER(IntKi)                         :: nVelNodes    ! Number of velocity nodes
  REAL(ReKi), DIMENSION(:), ALLOCATABLE  :: rVelNodes    ! Distance of velocity nodes from the first node - Used as a parameter for curve fitting
  REAL(ReKi)                             :: dRforceNodes ! Uniform distance between two consecutive force nodes
  REAL(ReKi), DIMENSION(:), ALLOCATABLE  :: rForceNodes  ! Distance of force nodes from the first node - Used to create the new force nodes through interpolation
  INTEGER(IntKI)                         :: i,j,k        ! Loop variables
  INTEGER(IntKI)                         :: jVelLower    ! Index of the vel node just smaller than the force node
  REAL(ReKi)                             :: rInterp      ! The location of this force node in (0,1) co-ordinates between the jVelLower and jVelLower+1 nodes

  nVelNodes = SIZE(velPositions,2)
  ALLOCATE(rVelNodes(nVelNodes), rForceNodes(numForcePts), STAT=ErrStat2)

  ! Calculate the distance of the velocity nodes from the first node
  rVelNodes(1) = 0.0 ! First node
  DO I=2,nVelNodes
     rVelNodes(I) = sqrt( (velPositions(1,I)-velPositions(1,1))*(velPositions(1,I)-velPositions(1,1)) + (velPositions(2,I)-velPositions(2,1))*(velPositions(2,I)-velPositions(2,1)) + (velPositions(3,I)-velPositions(3,1))*(velPositions(3,I)-velPositions(3,1)) )
  END DO

  dRforceNodes = rVelNodes(nVelNodes)/ REAL(numForcePts-1, ReKi) ! Calculate the uniform spacing between force nodes


  ! Now calculate the positions of the force nodes based on interpolation
  forceNodePositions(:,1) = velPositions(:,1)  ! First one is known
  forceNodePositions(:,numForcePts) = velPositions(:,nVelNodes)  ! Last one is known
  DO I=2,numForcePts-1 ! Calculate the position of the intermediate nodes
     rForceNodes(I) = REAL(I-1,ReKi) * dRforceNodes

     jVelLower=1
     do while ( (rVelNodes(jVelLower) - rForceNodes(I))*(rVelNodes(jVelLower+1) - rForceNodes(I)) .gt. 0 )
        jVelLower = jVelLower + 1
     end do
     rInterp =  (rForceNodes(I) - rVelNodes(jVelLower))/(rVelNodes(jVelLower+1)-rVelNodes(jVelLower)) ! The location of this force node in (0,1) co-ordinates between the jVelLower and jVelLower+1 nodes
     forceNodePositions(:,I) = velPositions(:,jVelLower) + rInterp * (velPositions(:,jVelLower+1) - velPositions(:,jVelLower))
  END DO

  DEALLOCATE(rVelNodes)
  DEALLOCATE(rForceNodes)

  RETURN

END SUBROUTINE CalcForceActuatorPositions



END MODULE OpenFOAM
!**********************************************************************************************************************************
