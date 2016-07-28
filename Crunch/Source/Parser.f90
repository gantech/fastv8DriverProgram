! Parse Module by: James Van Buskirk
! Modified by N. Weaver for GPP, 7/22/98
!
! o Handle added for data column references in parsed algebraic expression
!       for a primary and a secondary data array.
! o Added data arrays to get_value call
! o Added row argument to get_value formal call parameters
! o Removed "x" and "y" parameters from get_value call
! o Alternate handling added for non-instrinsic degree trig functions
! o Alternate symbol for exponentiation added ("^")
! o Added error messages string to argument list for tokenizer.
! o Added column dimension for data arrays
!        to argument list for tokenizer for error checking purposes.

! Modified by M. Buhl to port to Crunch, 06-Aug-1998.
!
! o Fixed value of Gamma constant.
! o Added the ATAN2D function.

!-------------
module ParsMod
!-------------
   use      DataMod
!  Define length of expression string to be parsed
   integer, parameter :: eqn_size = 512
!  Declare token type for operator and number stacks

   type(Token) x_eq, y_eq

   interface peek
      module procedure peek_char, peek_double
   end interface peek

   interface push
      module procedure push_char, push_double
   end interface push

   interface pop
      module procedure pop_char, pop_double
   end interface pop

!-------------
   contains
!-------------
      function peek_char(stack, depth)
         implicit none
         character(1) peek_char
         character(1), intent(in) :: stack(*)
         integer, intent(in) :: depth

         peek_char = stack(depth)
      end function peek_char
!=======================================================================
      function peek_double(stack, depth)
         implicit none
         real(DbKi) peek_double
         real(DbKi), intent(in) :: stack(*)
         integer, intent(in) :: depth

         peek_double = stack(depth)
      end function peek_double
!=======================================================================
      subroutine push_char(stack, depth, value)
         implicit none
         character(1), intent(inout) :: stack(*)
         integer, intent(inout) :: depth
         character(1), intent(in) :: value

         depth = depth+1
         stack(depth) = value
      end subroutine push_char
!=======================================================================
      subroutine push_double(stack, depth, value)
         implicit none
         real(DbKi), intent(inout) :: stack(*)
         integer, intent(inout) :: depth
         real(DbKi), intent(in) :: value

         depth = depth+1
         stack(depth) = value
      end subroutine push_double
!=======================================================================
      subroutine pop_char(stack, depth, value)
         implicit none
         character(1), intent(in) :: stack(*)
         integer, intent(inout) :: depth
         character(1), intent(out) :: value

         value = stack(depth)
         depth = depth-1
      end subroutine pop_char
!=======================================================================
      subroutine pop_double(stack, depth, value)
         implicit none
         real(DbKi), intent(in) :: stack(*)
         integer, intent(inout) :: depth
         real(DbKi), intent(out) :: value

         value = stack(depth)
         depth = depth-1
      end subroutine pop_double
!=======================================================================
      subroutine tokenize(equation, eqn, maxcol, error, errndx, errstr )

!        Routine to process an algebraic expression into tokens
!          with order of operations defined by a postfix stack.
!
!        In general, this parser scans the input string until it
!        encounters a null terminater.  Intermediate operations and
!        operands are placed on the "ops" stack.  When a end-of-expression
!        is reached all operations on the working stack are transferred
!        to the output token stack.  All operations and function calls are
!        converted to one character ASCII tokens in the output stack.
!
!        This parser recognizes "+,-,*,/,** or ^,_, and()" operations
!        as well as 32 instrinsic functions (including multiargument functions).
!
!        If an error occurs, an error message is loaded into string "errstr"
!        and the routine returns with the error flag set to .TRUE.

         implicit none
         character(*), intent(in) :: equation ! expression input string
         type(token), intent(out) :: eqn      ! output token postfix stack
         logical, intent(out) :: error
         integer i, j
         character(20) ctoken
         character(80), intent(out) :: errstr
         character(1)  dtoken
         character(7)  stoken
         character(1) ops(nops)
         character(1) lastop
         integer, intent(out) :: errndx
         integer depth, opdepth, numdepth, stdepth
         integer, intent(in) :: maxcol  ! maximum number of columns in data array.
         integer nargs
         integer ndigits

         stoken(1:1) = achar(128) ! 'Ç' token for ACOSD function
         stoken(2:2) = achar(129) ! 'ü' token for ASIND function
         stoken(3:3) = achar(130) ! 'é' token for ATAND function
         stoken(4:4) = achar(131) ! 'â' token for COSD function
         stoken(5:5) = achar(132) ! 'ä' token for SIND function
         stoken(6:6) = achar(133) ! 'à' token for TAND function
         dtoken(1:1) = achar(134) ! 'á' token for ATAN2D function

         i = 1
         error = .TRUE.
         errstr = ' '
         depth = 0
         opdepth = 0
         numdepth = 0
         stdepth = 0

         MainParse: do
            errndx = i
            GetToken: select case(equation(i:i))
            case (achar(0)) GetToken
               ! Found NULL indicating end of expression
            ! Now generate the equation stack
               if (depth == 0) then
               errstr = ' Your expression was interpreted as "empty".'
            return
               endif
               call pop(ops, depth, lastop)
               if (lastop /= '#') then
               errstr = ' Your expression is missing an operand.'
              return
               endif
               do while (depth > 0)
                  opdepth = opdepth+1
                  call pop(ops, depth, lastop)
                  if (index('+-*/^_', lastop) == 0) then
               errstr = ' Your expression seems to be missing one of the following "+-*/^_".'
              return
               endif
                  if (opdepth > nops) then
                  errstr = ' You have used too many operators in your expression.'
                 return
                  endif
                  eqn%ops(opdepth) = lastop
                  if (lastop /= '_') stdepth = stdepth-1
               end do
               opdepth = opdepth+1
               if (opdepth > nops) then
                 errstr = ' You have used too many operators in your expression.'
                 return
               endif
               eqn%ops(opdepth) = achar(0)
               if (stdepth /= 1) then
                 errstr = ' Your expression seems to have an extra operator or operand.'
                 return
               endif
               error = .FALSE.
               return
            case ('+') GetToken
               i = i+1
               if (depth == 0) then
                 errstr = ' Your expression seems to have a missing operand for a "+" operation.'
                 return
               endif
               call pop(ops, depth, lastop)
               if (lastop /= '#') then
                 errstr = ' Your expression seems to have a missing operand for a "+" operation.'
                 return
               endif
               do while (depth > 0)
                  call pop(ops, depth, lastop)
                  if (index('(,[', lastop) /= 0) then
                     call push(ops, depth, lastop)
                     exit
                  else if(index('+-*/^_', lastop) /= 0) then
                     opdepth = opdepth+1
                     if(opdepth > nops) then
                        errstr = ' You have used too many operators in your expression.'
                        return
                     endif
                     eqn%ops(opdepth) = lastop
                     if (lastop /= '_') stdepth = stdepth-1
                  else
                     errstr = ' Your expression seems to have an unused operand.'
                     return
                  end if
               end do
               if (depth >= nops) then
                 errstr = ' You have used too many operators in your expression.'
                 return
               endif
               call push(ops, depth, '+')
            case ('-') GetToken
               i = i+1
               if (depth == 0) then
                  call push(ops, depth, '_')
                  cycle MainParse
               else
                  call pop(ops, depth, lastop)
                  if (index('(,[', lastop) /= 0) then
                     if (depth+1 >= nops) then
                       errstr = ' You have used too many operators in your expression.'
                       return
                     endif
                     call push(ops, depth, lastop)
                     call push(ops, depth, '_')
                     cycle MainParse
                  else if (lastop /= '#') then
                     errstr = ' A "-" operation in your expression seems to be missing an operand.'
                     return
                  end if
               end if
               do while (depth > 0)
                  call pop(ops, depth, lastop)
                  if (index('(,[', lastop) /= 0) then
                     call push(ops, depth, lastop)
                     exit
                  else if(index('+-*/^_', lastop) /= 0) then
                     opdepth = opdepth+1
                     if(opdepth > nops) then
                       errstr = ' You have used too many operators in your expression.'
                       return
                     endif
                     eqn%ops(opdepth) = lastop
                     if (lastop /= '_') stdepth = stdepth-1
                  else
                     errstr = ' Your expression seems to have an unused operand.'
                     return
                  end if
               end do
               if (depth >= nops) then
                   errstr = ' You have used too many operators in your expression.'
                   return
               endif
               call push(ops, depth, '-')

            case ('^') GetToken  ! exponentiation alternate
               i = i+1
                  if (depth == 0) then
                      errstr = ' Your expression is missing an argument for a "^" operation.'
                      return
                  endif
                  call pop(ops, depth, lastop)
                  if (lastop /= '#') then
                     errstr = ' Your expression is missing an argument for a "^" operation.'
                     return
                  end if
                  do while (depth > 0)
                     call pop(ops, depth, lastop)
                     if (index('(,[+-*/^_', lastop) /= 0) then
                        call push(ops, depth, lastop)
                        exit
                     else
                        errstr = ' Your expression seems to have an extra operand.'
                        return
                     end if
                  end do
                  if (depth >= nops) then
                      errstr = ' You have used too many operators in your expression.'
                      return
                  endif
                  call push(ops, depth, '^')

            case ('*') GetToken
               i = i+1
               if(equation(i:i) == '*') then ! case('**') GetToken
                  i = i+1
                  if (depth == 0) then
                      errstr = ' Your expression is missing an argument for a "**" operation.'
                      return
                  endif
                  call pop(ops, depth, lastop)
                  if (lastop /= '#') then
                     errstr = ' Your expression is missing an argument for a "**" operation.'
                     return
                  end if
                  do while (depth > 0)
                     call pop(ops, depth, lastop)
                     if (index('(,[+-*/^_', lastop) /= 0) then
                        call push(ops, depth, lastop)
                        exit
                     else
                        errstr = ' Your expression seems to have an extra operand.'
                        return
                     end if
                  end do
                  if (depth >= nops) then
                      errstr = ' You have used too many operators in your expression.'
                      return
                  endif
                  call push(ops, depth, '^')
               else ! case('*') GetToken
                  if (depth == 0) then
                      errstr = ' Your expression is missing an operand for a "*" operation.'
                      return
                  endif
                  call pop(ops, depth, lastop)
                  if (lastop /= '#') then
                     errstr = ' Your expression is missing an operand for a "*" operation.'
                     return
                  end if
                  do while (depth > 0)
                     call pop(ops, depth, lastop)
                     if (index('(,[+-_', lastop) /= 0) then
                        call push(ops, depth, lastop)
                        exit
                     else if(index('*/^', lastop) /= 0) then
                        opdepth = opdepth+1
                        if(opdepth > nops) then
                           errstr = ' You have used too many operators in your expression.'
                           return
                        endif
                        eqn%ops(opdepth) = lastop
                        stdepth = stdepth-1
                     else
                        errstr = ' Your expression seems to have an extra operand.'
                        return
                     end if
                  end do
                  if (depth >= nops) then
                     errstr = ' You have used too many operators in your expression.'
                     return
                  endif
                  call push(ops, depth, '*')
               end if
            case ('/') GetToken
               i = i+1
               if (depth == 0) then
                  errstr = ' Your expression is missing an operand for a "/" operation.'
                  return
               endif
               call pop(ops, depth, lastop)
               if (lastop /= '#') then
                  errstr = ' Your expression is missing an operand for a "/" operation.'
                  return
               end if
               do while (depth > 0)
                  call pop(ops, depth, lastop)
                  if (index('(,[+-_', lastop) /= 0) then
                     call push(ops, depth, lastop)
                     exit
                  else if(index('*/^', lastop) /= 0) then
                     opdepth = opdepth+1
                     if(opdepth > nops) then
                        errstr = ' You have used too many operators in your expression.'
                        return
                     endif
                     eqn%ops(opdepth) = lastop
                     stdepth = stdepth-1
                  else
                     errstr = ' Your expression seems to have an extra operand.'
                     return
                  end if
               end do
               if (depth >= nops) then
                  errstr = ' You have used too many operators in your expression.'
                  return
               endif
               call push(ops, depth, '/')
            case ('(') GetToken
               i = i+1
               if (depth == 0) then
                  call push(ops, depth, '(')
                  cycle MainParse
               else
                  lastop = peek(ops, depth)
                  if (index('(,[+-*/^_', lastop) /= 0) then
                     if(depth >= nops) then
                        errstr = ' You have used too many operators in your expression.'
                        return
                     endif
                     call push(ops, depth, '(')
                  else if (lastop == '#') then
                     errstr = ' Your expression seems to have an extra operand.'
                     return
                  else
                     if(depth >= nops) then
                        errstr = ' You have used too many operators in your expression.'
                        return
                     endif
                     call push(ops, depth, '[')
                  end if
               end if
            case (',') GetToken
               i = i+1
               if (depth == 0) then
                  errstr = ' A function in your expression seems to be missing an argument.'
                  return
               endif
               call pop(ops, depth, lastop)
               if (lastop /= '#') then
                  errstr = ' A function in your expression seems to be missing an argument.'
                  return
               end if
               do while (depth > 0)
                  call pop(ops, depth, lastop)
                  if (index(',[', lastop) /= 0) then
                     call push(ops, depth, lastop)
                     call push(ops, depth, ',')
                     cycle MainParse
                  else if(index('+-*/^_', lastop) /= 0) then
                     opdepth = opdepth+1
                     if(opdepth > nops) then
                        errstr = ' You have used too many operators in your expression.'
                        return
                     endif
                     eqn%ops(opdepth) = lastop
                     if (lastop /= '_') stdepth = stdepth-1
                  else
                     errstr = ' A function in your expression seems to have an extra argument.'
                     return
                  end if
               end do
                     errstr = ' A function in your expression seems to be missing an argument.'
               return
            case (')') GetToken
               i = i+1
               if (depth == 0) then
                 errstr = ' Your expression seems to have an unmatched ")".'
                  return
               endif
               call pop(ops, depth, lastop)
               if (lastop /= '#') then
                  errstr = ' While resolving a "()" subexpression, a missing operand was detected.'
                  return
               end if
               nargs = 1
               do while (depth > 0)
                  call pop(ops, depth, lastop)
                  if (index('+-*/^_', lastop) /= 0) then
                     opdepth = opdepth+1
                     if (opdepth > nops) then
                        errstr = ' You have used too many operators in your expressions.'
                        return
                     endif
                     eqn%ops(opdepth) = lastop
                     if (lastop /= '_') stdepth = stdepth-1
                  else if (lastop == ',') then
                     nargs = nargs+1
                  else if (lastop == '(') then
                     if (nargs > 1) then
                        errstr = ' Your expression appears to have an unbalanced "(".'
                        return
                     endif
                     call push(ops, depth, '#')
                     cycle MainParse
                  else if (lastop == '[') then
                     if (depth == 0) then
                       errstr = ' Your expression appears to have an empty "()" subexpression.'
                       return
                     endif
                     call pop(ops, depth, lastop)
                     if (index('@A', lastop) /= 0) then
                        numdepth = numdepth+1
                        if (numdepth > nconst) then
                           errstr = ' You have used too many numbers in your expression.'
                           return
                        endif
                        eqn%nums(numdepth) = nargs
                        if(opdepth+1 >= nops) then
                           errstr = ' You have used too many operators in your expression.'
                           return
                        endif
                        eqn%ops(opdepth+1) = 'n'
                        if (stdepth+1 > nconst) then
                           errstr = ' You have used too many numbers in your expression.'
                           return
                        endif
                        opdepth = opdepth+2
                        eqn%ops(opdepth) = lastop
                        stdepth = stdepth-nargs+1
                        call push(ops, depth, '#')
                        cycle MainParse
                     else if(index('BCDEFGHIJKLMNOPQST'//stoken, lastop) /= 0) then
                        if (nargs /= 1) then
                           errstr = ' A single argument function contains an incorrect number of arguments.'
                           return
                        endif
                        opdepth = opdepth+1
                        if(opdepth > nops) then
                           errstr = ' You have used too many operators in your expression.'
                           return
                        endif
                        eqn%ops(opdepth) = lastop
                        call push(ops, depth, '#')
                        cycle MainParse
                     else if(index('RUVWXZ'//dtoken, lastop) /= 0) then
                        if (nargs /= 2) then
                           errstr = ' A double argument function contains an incorrect number of arguments.'
                           return
                        endif
                        opdepth = opdepth+1
                        if(opdepth > nops) then
                           errstr = ' Parsing two argument function: Maximum operator stack depth exceeded.'
                           return
                        endif
                        eqn%ops(opdepth) = lastop
                        stdepth = stdepth-1
                        call push(ops, depth, '#')
                        cycle MainParse
                     else
!                        stop 'Internal error in Tokenize: '')'''
                        errstr = ' Your expression has an unresolvable "()" subexpression.'
                        return
                     end if
                  else
 !                    stop 'Internal error in Tokenize: '')'''
                     errstr = ' Your expression has an unresolvable "()" subexpression.'
                     return
                  end if
               end do
               errstr = ' Your expression has an unresolvable "()" subexpression.'
               return
            case ('A':'Z', 'a':'z') GetToken
               if (depth > 0) then
                  if (index('+-*/^_(,[', peek(ops, depth)) == 0) then
                     errstr = ' Parsing a function: Missing operator of "+-*/^_(,[" .'
                     return
                  endif
                  if (depth >= nops) then
                     errstr = ' Parsing a function: Maximum operator stack depth exceeded.'
                     return
                  endif
               end if
               ctoken = ''
               ParseToken: do
                  GetLetter: select case(equation(i:i))
                  case('A':'Z', '_', '$', '0':'9') GetLetter
                     if (len_trim(ctoken) >= 20) then
                        errstr = ' A function name appears to have more than 20 characters.'
                        return
                     endif
                     ctoken = trim(ctoken) // equation(i:i)
                  case('a':'z') GetLetter
                     if (len_trim(ctoken) >= 20) then
                        errstr = ' A function name appears to have more than 20 characters.'
                        return
                     endif
                     ctoken = trim(ctoken) // achar(iachar(equation(i:i))-32)
                  case default GetLetter
                     if (trim(ctoken) == 'MAX') then
                        call push(ops, depth, '@')
                     else if(trim(ctoken) == 'MIN') then
                        call push(ops, depth, 'A')
                     else if(trim(ctoken) == 'ACOS') then
                        call push(ops, depth, 'B')
                     else if(trim(ctoken) == 'ASIN') then
                        call push(ops, depth, 'C')
                     else if(trim(ctoken) == 'ATAN') then
                        call push(ops, depth, 'D')
                     else if(trim(ctoken) == 'ACOSD') then
                        call push(ops, depth, achar(128) ) ! Ç
                     else if(trim(ctoken) == 'ASIND') then
                        call push(ops, depth, achar(129) )! ' ü
                     else if(trim(ctoken) == 'ATAND') then
                        call push(ops, depth, achar(130) )! ' é
                     else if(trim(ctoken) == 'COS') then
                        call push(ops, depth, 'E')
                     else if(trim(ctoken) == 'COSD') then
                        call push(ops, depth, achar(131) )! ' â
                     else if(trim(ctoken) == 'COSH') then
                        call push(ops, depth, 'F')
                     else if(trim(ctoken) == 'EXP') then
                        call push(ops, depth, 'G')
                     else if(trim(ctoken) == 'LOG') then
                        call push(ops, depth, 'H')
                     else if(trim(ctoken) == 'LOG10') then
                        call push(ops, depth, 'I')
                     else if(trim(ctoken) == 'SIN') then
                        call push(ops, depth, 'J')
                     else if(trim(ctoken) == 'SIND') then
                        call push(ops, depth, achar(132) ) ! ä
                     else if(trim(ctoken) == 'SINH') then
                        call push(ops, depth, 'K')
                     else if(trim(ctoken) == 'SQRT') then
                        call push(ops, depth, 'L')
                     else if(trim(ctoken) == 'TAN') then
                        call push(ops, depth, 'M')
                     else if(trim(ctoken) == 'TAND') then
                        call push(ops, depth, achar(133)  ) ! à
                     else if(trim(ctoken) == 'TANH') then
                        call push(ops, depth, 'N')
                     else if(trim(ctoken) == 'ABS') then
                        call push(ops, depth, 'O')
                     else if(trim(ctoken) == 'INT') then
                        call push(ops, depth, 'P')
                     else if(trim(ctoken) == 'NINT') then
                        call push(ops, depth, 'Q')
                     else if(trim(ctoken) == 'ATAN2') then
                        call push(ops, depth, 'R')
                     else if(trim(ctoken) == 'ATAN2D') then
                        call push(ops, depth, achar(134)  ) ! á
                     else if(trim(ctoken) == 'CEILING') then
                        call push(ops, depth, 'S')
                     else if(trim(ctoken) == 'FLOOR') then
                        call push(ops, depth, 'T')
                     else if(trim(ctoken) == 'MOD') then
                        call push(ops, depth, 'U')
                     else if(trim(ctoken) == 'MODULO') then
                        call push(ops, depth, 'V')
                     else if(trim(ctoken) == 'DIM') then
                        call push(ops, depth, 'W')
                     else if(trim(ctoken) == 'SIGN') then
                        call push(ops, depth, 'X')
                     else if(trim(ctoken) == 'ROOT') then
                        call push(ops, depth, 'Z')
                     else if(trim(ctoken) == 'E') then
                        call push(ops, depth, '#')
                        opdepth = opdepth+1
                        if (opdepth > nops) then
                           errstr = ' Your expression has used too many operators.'
                           return
                        endif
                        eqn%ops(opdepth) = 'e'
                        stdepth = stdepth+1
                        if (stdepth > nconst) then
                           errstr = ' Your expression has used too many numbers.'
                           return
                        endif
                     else if(trim(ctoken) == 'GAMMA') then
                        call push(ops, depth, '#')
                        opdepth = opdepth+1
                        if (opdepth > nops) then
                           errstr = ' Your expression has used too many operators.'
                           return
                        endif
                        eqn%ops(opdepth) = 'g'
                        stdepth = stdepth+1
                        if (stdepth > nconst) then
                           errstr = ' Your expression has used too many numbers.'
                           return
                        endif
                     else if(trim(ctoken) == 'PI') then
                        call push(ops, depth, '#')
                        opdepth = opdepth+1
                        if (opdepth > nops) then
                           errstr = ' Your expression has used too many operators.'
                           return
                        endif
                        eqn%ops(opdepth) = 'p'
                        stdepth = stdepth+1
                        if (stdepth > nconst) then
                           errstr = ' Your expression has used too many numbers.'
                           return
                        endif
                     else if(trim(ctoken) == 'RAND') then
                        call push(ops, depth, '#'  ) ! æ
                        opdepth = opdepth+1
                        if (opdepth > nops) then
                           errstr = ' Your expression has used too many operators.'
                           return
                        endif
                        eqn%ops(opdepth) = 'r'
                        stdepth = stdepth+1
                        if (stdepth > nconst) then
                           errstr = ' Your expression has used too many numbers.'
                           return
                        endif
                     else if(trim(ctoken) == 'X') then
                        call push(ops, depth, '#')
                        opdepth = opdepth+1
                        if (opdepth > nops) then
                           errstr = ' Your expression has used too many operators.'
                           return
                        endif
                        eqn%ops(opdepth) = 'x'
                        stdepth = stdepth+1
                        if (stdepth > nconst) then
                           errstr = ' Your expression has used too many numbers.'
                           return
                        endif
                     else if(trim(ctoken) == 'Y') then
                        call push(ops, depth, '#')
                        opdepth = opdepth+1
                        if (opdepth > nops) then
                           errstr = ' Your expression has used too many operators.'
                           return
                        endif
                        eqn%ops(opdepth) = 'y'
                        stdepth = stdepth+1
                        if (stdepth > nconst) then
                           errstr = ' Your expression has used too many numbers.'
                           return
                        endif
                     else if(ctoken(1:1) == 'C') then
                        call push(ops, depth, '#')
                        opdepth = opdepth+1
                        if (opdepth > nops) then
                           errstr = ' Your expression has used too many operators.'
                           return
                        endif
                        eqn%ops(opdepth) = 'c'
                        stdepth = stdepth+1
                        if (stdepth > nconst) then
                           errstr = ' Your expression has used too many numbers.'
                           return
                        endif
                        numdepth = numdepth+1
                        if (numdepth > nconst) then
                           errstr = ' Your expression has used too many numbers.'
                           return
                        endif
                        if (index('0123456789', ctoken(2:2)) == 0) then
                           errstr = ' Your expression seems to be making an invalid data column reference.'
                           return
                        endif
                        read(ctoken(2:len(ctoken)), *) eqn%nums(numdepth)
                        if ( eqn%nums(numdepth) > maxcol) then
                           errstr = ' Your expression seems to be making an invalid data column reference.'
                           return
                        endif
                     else
                        errstr = ' The string "'//trim(ctoken)//'" was not understood in your expression.'
                        return ! Unrecognized token
                     end if
                     exit ParseToken
                  end select GetLetter
                  i = i+1
               end do ParseToken
            case ('0':'9', '.') GetToken
               if (depth /= 0) then
                  if (index('+-*/^_(,[', peek(ops, depth)) == 0) then
                      errstr = ' Your expression seems to be missing an operator.'
                      return
                   endif
               end if
               j = i
               ndigits = 0
               if (equation(i:i) /= '.') then
                  do
                     i = i+1
                     ndigits = ndigits+1
                     if (index('0123456789', equation(i:i)) == 0) then
                        if (index('.deDE', equation(i:i)) /= 0) exit
                        numdepth = numdepth+1
                        if (numdepth > nconst) then
                           errstr = ' Your expression contains too many numbers.'
                           return
                        endif
                        read(equation(j:i-1), *) eqn%nums(numdepth)
                        opdepth = opdepth+1
                        if (opdepth > nops) then
                           errstr = ' Your expression contains too many operators.'
                           return
                        endif
                        eqn%ops(opdepth) = 'n'
                        stdepth = stdepth+1
                        if (stdepth > nconst) then
                           errstr = ' Your expression contains too many numbers.'
                           return
                        endif
                        if (depth >= nops) then
                           errstr = ' Your expression contains too many operators.'
                           return
                        endif
                        call push(ops, depth, '#')
                        cycle MainParse
                     end if
                  end do
               end if
               if (equation(i:i) == '.') then
                  do
                     i = i+1
                     if (index('0123456789', equation(i:i)) == 0) then
                        if (index('deDE', equation(i:i)) /= 0) exit
                        if (ndigits == 0) then
                           errstr = ' Your expression appears to contain a number with missing digits.'
                           return
                        endif
                        numdepth = numdepth+1
                        if (numdepth > nconst) then
                           errstr = ' Your expression contains too many numbers.'
                           return
                        endif
                        read(equation(j:i-1), *) eqn%nums(numdepth)
                        opdepth = opdepth+1
                        if (opdepth > nops) then
                           errstr = ' Your expression contains too many operators.'
                           return
                        endif
                        eqn%ops(opdepth) = 'n'
                        stdepth = stdepth+1
                        if (stdepth > nconst) then
                           errstr = ' Your expression contains too many numbers.'
                           return
                        endif
                        if (depth >= nops) then
                           errstr = ' Your expression contains too many operators.'
                           return
                        endif
                        call push(ops, depth, '#')
                        cycle MainParse
                     end if
                     ndigits = ndigits+1
                  end do
               end if
               if (ndigits == 0) then
                   errstr = ' Your expression appears to contain a number with missing digits.'
                   return
               endif
               i = i+1
               if (index('+-', equation(i:i)) /= 0) i = i+1
               ndigits = 0
               do
                  if (index('0123456789', equation(i:i)) == 0) then
                     if (ndigits == 0) then
                        errstr = ' Your expression appears to contain a number with missing digits.'
                        return
                     endif
                     numdepth = numdepth+1
                     if (numdepth > nconst) then
                        errstr = ' Your expression contains too many numbers.'
                        return
                     endif
                     read(equation(j:i-1), *) eqn%nums(numdepth)
                     opdepth = opdepth+1
                     if (opdepth > nops) then
                        errstr = ' Your expression contains too many operators.'
                        return
                     endif
                     eqn%ops(opdepth) = 'n'
                     stdepth = stdepth+1
                     if (stdepth > nconst) then
                        errstr = ' Your expression contains too many numbers.'
                        return
                     endif
                     if (depth >= nops) then
                        errstr = ' Your expression contains too many operators.'
                        return
                     endif
                     call push(ops, depth, '#')
                     cycle MainParse
                  end if
                  i = i+1
                  ndigits = ndigits+1
               end do
            case (' ', achar(9), achar(10), achar(13)) GetToken
               i = i+1
            case default GetToken
               errstr = ' Your expression contains an unrecognized operator.'
               return
            end select GetToken
         end do MainParse
      end subroutine tokenize
!=======================================================================
      subroutine GetValue(Eqn, ir, IFi, value, error, errstr)

!        If an error occurs, an error message is loaded into string "errstr"
!        and the routine returns with the error flag set to .TRUE.

         use      DataMod
         use      SysSubs

         implicit none
         type(token), intent(in) :: eqn
         real(DbKi) x, y
         real(DbKi), intent(out) :: value
         logical, intent(out)       :: error
         character(80), intent(out) :: errstr
         integer, intent(in) :: ir, IFi
         integer i
         real(DbKi) val1, val2
         real(DbKi) cref, cval
         integer depth
         integer n, j
         integer cnum
         real(DbKi) eq_stack(nconst)
         real real_val

         errstr = ' '
         depth = 0
         cnum = 1
         do i = 1, nops
            GetOp: select case(eqn%ops(i))
            case (achar(0)) GetOp
               error = .FALSE.
               call pop(eq_stack, depth, value)
               return
            case ('+') GetOp
               call pop(eq_stack, depth, val2)
               call pop(eq_stack, depth, val1)
               val1 = val1+val2
               if (Is_Nan(val1)) then
                  errstr = ' Your expression produced an invalid result detected during a "+" operation.'
                  error = .TRUE.
                  return
               end if
               call push(eq_stack, depth, val1)
            case ('-') GetOp
               call pop(eq_stack, depth, val2)
               call pop(eq_stack, depth, val1)
               val1 = val1-val2
               if (Is_Nan(val1)) then
                  errstr = ' Your expression produced an invalid result detected during a "-" operation.'
                  error = .TRUE.
                  return
               end if
               call push(eq_stack, depth, val1)
            case ('*') GetOp
               call pop(eq_stack, depth, val2)
               call pop(eq_stack, depth, val1)
               val1 = val1*val2
               if (Is_Nan(val1)) then
                  errstr = ' Your expression produced an invalid result detected during a "*" operation.'
                  error = .TRUE.
                  return
               end if
               call push(eq_stack, depth, val1)
            case ('/') GetOp
               call pop(eq_stack, depth, val2)
               call pop(eq_stack, depth, val1)
               if (val2 == 0) then
                  errstr = ' Your expression produced a "divide by zero" error.'
                  error = .TRUE.
                  return
               end if
               val1 = val1/val2
               if (Is_Nan(val1)) then
                  errstr = ' Your expression produced an invalid result detected during a "/" operation.'
                  error = .TRUE.
                  return
               end if
               call push(eq_stack, depth, val1)
            case ('^') GetOp
               call pop(eq_stack, depth, val2)
               call pop(eq_stack, depth, val1)
               if (val1 == 0) then
                  if (val2 <= 0) then
                     errstr = ' Your expression attempts to raise zero to a negative power.'
                     error = .TRUE.
                     return
                  else
                     call push(eq_stack, depth, val1)
                  end if
               else if(val1 < 0) then
                  if (modulo(val2, 1.0_DbKi) /= 0) then
                     errstr = ' Your expression attempts to take the even root of a negative number.'
                     error = .TRUE.
                     return
                  else if(modulo(val2, 2.0_DbKi) == 0) then
                     val1 = abs(val1)**val2
                     if (Is_Nan(val1)) then
                        errstr = ' Your expression produced an invalid result detected during a "^" operation.'
                        error = .TRUE.
                        return
                     end if
                     call push(eq_stack, depth, val1)
                  else
                     val1 = sign(abs(val1)**val2, val1)
                     if (Is_Nan(val1)) then
                        errstr = ' Your expression produced an invalid result detected during a "^" operation.'
                        error = .TRUE.
                        return
                     end if
                     call push(eq_stack, depth, val1)
                  end if
               else
                  val1 = val1**val2
                  if (Is_Nan(val1)) then
                     errstr = ' Your expression produced an invalid result detected during a "^" operation.'
                     error = .TRUE.
                     return
                  end if
                  call push(eq_stack, depth, val1)
               end if
            case ('_') GetOp ! Unary negation
               call pop(eq_stack, depth, val1)
               val1 = -val1
               call push(eq_stack, depth, val1)
            case ('@') GetOp ! max
               call pop(eq_stack, depth, val1)
               n = val1
               call pop(eq_stack, depth, val1)
               do j = 2, n
                  call pop(eq_stack, depth, val2)
                  val1 = max(val1,val2)
               end do
               call push(eq_stack, depth, val1)
            case ('A') GetOp ! min
               call pop(eq_stack, depth, val1)
               n = val1
               call pop(eq_stack, depth, val1)
               do j = 2, n
                  call pop(eq_stack, depth, val2)
                  val1 = min(val1,val2)
               end do
               call push(eq_stack, depth, val1)
            case ('B') GetOp ! acos
               call pop(eq_stack, depth, val1)
               if (abs(val1) > 1) then
                  errstr = ' Your expression attempts to evaluate the arccosine of a number > 1.'
                  error = .TRUE.
                  return
               end if
               val1 = acos(val1)
               call push(eq_stack, depth, val1)
            case ('C') GetOp ! asin
               call pop(eq_stack, depth, val1)
               if (abs(val1) > 1) then
                  errstr = ' Your expression attempts to evaluate the arcsine of a number > 1.'
                  error = .TRUE.
                  return
               end if
               val1 = asin(val1)
               call push(eq_stack, depth, val1)
            case ('D') GetOp ! atan
               call pop(eq_stack, depth, val1)
               val1 = atan(val1)
               call push(eq_stack, depth, val1)
            case ('E') GetOp ! cos
               call pop(eq_stack, depth, val1)
               if (abs(val1) > 1.0e15) then
                  errstr = ' Your expression attempts to evaluate the cosine of a number > 1.0e15.'
                  error = .TRUE.
                  return
               end if
               val1 = cos(val1)
               call push(eq_stack, depth, val1)
            case ('F') GetOp ! cosh
               call pop(eq_stack, depth, val1)
               if (abs(val1) > 691) then
                  errstr = ' Your expression attempts to evaluate the COSH of a number > 691.'
                  error = .TRUE.
                  return
               end if
               val1 = cosh(val1)
               call push(eq_stack, depth, val1)
            case ('G') GetOp ! exp
               call pop(eq_stack, depth, val1)
               if (abs(val1) > 690) then
                  errstr = ' Your expression attempts to evaluate EXP(x) where x is a number > 690.'
                  error = .TRUE.
                  return
               end if
               val1 = exp(val1)
               call push(eq_stack, depth, val1)
            case ('H') GetOp ! log
               call pop(eq_stack, depth, val1)
               if (val1 <= 0) then
                  errstr = ' Your expression attempts to take the natural log of a negative number.'
                  error = .TRUE.
                  return
               end if
               val1 = log(val1)
               call push(eq_stack, depth, val1)
            case ('I') GetOp ! log10
               call pop(eq_stack, depth, val1)
               if (val1 <= 0) then
                  errstr = ' Your expression attempts to take the common log of a negative number.'
                  error = .TRUE.
                  return
               end if
               val1 = log10(val1)
               call push(eq_stack, depth, val1)
            case ('J') GetOp ! sin
               call pop(eq_stack, depth, val1)
               if (abs(val1) > 1.0e15) then
                  errstr = ' Your expression attempts to evaluate the sine of a number > 1.0e15.'
                  error = .TRUE.
                  return
               end if
               val1 = sin(val1)
               call push(eq_stack, depth, val1)
            case ('K') GetOp ! sinh
               call pop(eq_stack, depth, val1)
               if (abs(val1) > 691) then
                  errstr = ' Your expressions attempts to evaluate the SINH of a number > 691.'
                  error = .TRUE.
                  return
               end if
               val1 = sinh(val1)
               call push(eq_stack, depth, val1)
            case ('L') GetOp ! sqrt
               call pop(eq_stack, depth, val1)
               if (val1 < 0) then
                  errstr = ' Your expression attempts to evaluate the SQRT of a negative number.'
                  error = .TRUE.
                  return
               end if
               val1 = sqrt(val1)
               call push(eq_stack, depth, val1)
            case ('M') GetOp ! tan
               call pop(eq_stack, depth, val1)
               if (abs(val1) > 1.0e15) then
                  errstr = ' Your expression attempts to evaluate the tangent of a number > 1.0e15.'
                  error = .TRUE.
                  return
               end if
               val2 = cos(val1)
               val1 = sin(val1)
               if (val2 == 0) then
                  errstr = ' A "Divide by zero" was detected while attempting to evaluate the tangent.'
                  error = .TRUE.
                  return
               end if
               val1 = val1/val2
               if (Is_Nan(val1)) then
                  errstr = ' An invalid result detected while evaluating TAN function.'
                  error = .TRUE.
                  return
               end if
               call push(eq_stack, depth, val1)
            case ('N') GetOp ! tanh
               call pop(eq_stack, depth, val1)
               val1 = tanh(val1)
               call push(eq_stack, depth, val1)
            case ('O') GetOp ! abs
               call pop(eq_stack, depth, val1)
               val1 = abs(val1)
               call push(eq_stack, depth, val1)
            case ('P') GetOp ! int
               call pop(eq_stack, depth, val1)
               val1 = aint(val1)
               call push(eq_stack, depth, val1)
            case ('Q') GetOp ! nint
               call pop(eq_stack, depth, val1)
               val1 = anint(val1)
               call push(eq_stack, depth, val1)
            case ('R') GetOp ! atan2
               call pop(eq_stack, depth, val2)
               call pop(eq_stack, depth, val1)
               if (val1 == 0 .AND. val2 == 0) then
                  errstr = ' A zero value was detected for both arguments of the ATAN2 function.'
                  error = .TRUE.
                  return
               end if
               val1 = atan2(val1, val2)
               call push(eq_stack, depth, val1)
            case ('S') GetOp ! ceiling
               call pop(eq_stack, depth, val1)
               val1 = ceiling(val1)
               call push(eq_stack, depth, val1)
            case ('T') GetOp ! floor
               call pop(eq_stack, depth, val1)
               val1 = floor(val1)
               call push(eq_stack, depth, val1)
            case ('U') GetOp ! mod
               call pop(eq_stack, depth, val2)
               call pop(eq_stack, depth, val1)
               if (val2 == 0) then
                  errstr = ' Your expression uses zero for the second argument in MOD function.'
                  error = .TRUE.
                  return
               end if
               val1 = mod(val1, val2)
               call push(eq_stack, depth, val1)
            case ('V') GetOp ! modulo
               call pop(eq_stack, depth, val2)
               call pop(eq_stack, depth, val1)
               if (val2 == 0) then
                  errstr = ' Your expression uses zero for the second argument in MODULO function.'
                  error = .TRUE.
                  return
               end if
               val1 = modulo(val1, val2)
               call push(eq_stack, depth, val1)
            case ('W') GetOp ! dim
               call pop(eq_stack, depth, val2)
               call pop(eq_stack, depth, val1)
               val1 = dim(val1, val2)
               call push(eq_stack, depth, val1)
            case ('X') GetOp ! sign
               call pop(eq_stack, depth, val2)
               call pop(eq_stack, depth, val1)
               val1 = sign(val1, val2)
               call push(eq_stack, depth, val1)
            case ('Z') GetOp ! root
               call pop(eq_stack, depth, val2)
               call pop(eq_stack, depth, val1)
               if (val1 == 0) then
                  if (val2 <= 0) then
                     errstr = ' Your expression attempts to take negative root of zero.'
                     error = .TRUE.
                     return
                  else
                     call push(eq_stack, depth, val1)
                  end if
               else if(val1 < 0) then
                  if (modulo(val2, 1.0_DbKi) /= 0) then
                     errstr = ' Your expression attempts to take the even root of a negative number.'
                     error = .TRUE.
                     return
                  else if(modulo(val2, 2.0_DbKi) == 0) then
                     errstr = ' Your expression attempts to take the even root of a negative number.'
                     error = .TRUE.
                     return
                  else
                     if (Is_Nan(1/val2)) then
                        errstr = ' An invalid exponent was detected while evaluating ROOT function.'
                        error = .TRUE.
                        return
                     end if
                     val1 = -abs(val1)**(1/val2)
                     if (Is_Nan(val1)) then
                        errstr = ' An invalid result was detected while evaluating ROOT function.'
                        error = .TRUE.
                        return
                     end if
                     call push(eq_stack, depth, val1)
                  end if
               else
                  if (val2 == 0) then
                     errstr = ' Zero was detected for an exponent while evaluating ROOT function.'
                     error = .TRUE.
                     return
                  end if
                  if (Is_Nan(1/val2)) then
                     errstr = ' An invalid exponent was detected while evaluating ROOT function.'
                     error = .TRUE.
                     return
                  end if
                  val1 = val1**(1/val2)
                  if (Is_Nan(val1)) then
                     errstr = ' An invalid result was detected while evaluating ROOT function.'
                     error = .TRUE.
                     return
                  end if
                  call push(eq_stack, depth, val1)
               end if
            case ('c') GetOp ! c - next column element reference
               cref = eqn%nums(cnum)
            cval = ConvData( int(cref), ir, IFi )
               call push(eq_stack, depth, cval)
               cnum = cnum+1
           case ('e') GetOp ! e
               call push(eq_stack, depth, 2.71828182845904523536_DbKi)
            case ('g') GetOp ! gamma
               call push(eq_stack, depth, 0.57721566490153286061_DbKi)
            case ('n') GetOp ! next constant
               call push(eq_stack, depth, eqn%nums(cnum))
               cnum = cnum+1
            case ('p') GetOp ! pi
               call push(eq_stack, depth, 3.14159265358979323846_DbKi)
            case ('r') GetOp ! rand
               call random_number(real_val)
               call push(eq_stack, depth, real(real_val,DbKi))
            case ('x') GetOp ! x
               call push(eq_stack, depth, x)
            case ('y') GetOp ! y
               call push(eq_stack, depth, y)
            case ( achar(128) ) GetOp ! ACOSD
               call pop(eq_stack, depth, val1)
               if (abs(val1) > 1) then
                  errstr = ' Your expression attempts to evaluate the arccosine (ACOSD) of a number > 1.'
                  error = .TRUE.
                  return
               end if
               val1 = ACOSD(val1)
               call push(eq_stack, depth, val1)
            case ( achar(129) ) GetOp ! ASIND
               call pop(eq_stack, depth, val1)
               if (abs(val1) > 1) then
                  errstr = ' Your expression attempts to evaluate the arcsine (ASIND) of a number > 1.'
                  error = .TRUE.
                  return
               end if
               val1 = ASIND(val1)
               call push(eq_stack, depth, val1)
            case ( achar(130) ) GetOp ! ATAND
               call pop(eq_stack, depth, val1)
               val1 = ATAND(val1)
               call push(eq_stack, depth, val1)
            case ( achar(131) ) GetOp ! COSD
               call pop(eq_stack, depth, val1)
               if (abs(val1) > 1.0e15) then
                  errstr = ' Your expression attempts to evaluate the cosine (COSD) of a number > 1.0e15'
                  error = .TRUE.
                  return
               end if
               val1 = COSD(val1)
               call push(eq_stack, depth, val1)
            case ( achar(132)) GetOp ! SIND
               call pop(eq_stack, depth, val1)
               if (abs(val1) > 1.0e15) then
                  errstr = ' Your expression attempts to evaluate the sine (SIND) of a number > 1.0e15'
                  error = .TRUE.
                  return
               end if
               val1 = SIND(val1)
               call push(eq_stack, depth, val1)
            case ( achar(133) ) GetOp ! TAND
               call pop(eq_stack, depth, val1)
               val1 = TAND(val1)
               call push(eq_stack, depth, val1)
            case ( achar(134) ) GetOp ! ATAN2D
               call pop(eq_stack, depth, val2)
               call pop(eq_stack, depth, val1)
               if (val1 == 0 .AND. val2 == 0) then
                  errstr = ' A zero value was detected for both arguments of the ATAN2D function.'
                  error = .TRUE.
                  return
               end if
               val1 = 180.0_DbKi*atan2(val1, val2)/3.14159265358979323846_DbKi
               call push(eq_stack, depth, val1)
            end select GetOp
         end do
      end subroutine GetValue
!=======================================================================
!     Provide equivalent to nonstandard SIND function using standard SIN call
!     Provide same for COSD, TAND, ASIND, ACOSD, and ATAND
!=======================================================================
      double precision function SIND( ANGLE )
         implicit none
         real(DbKi), intent(in) :: ANGLE
         real(DbKi)  PI

         PI = 3.14159265358979323846_DbKi
         SIND = DSIN ( ANGLE * PI / 180.0 )
         return
      end function SIND
!=======================================================================
      double precision function COSD( ANGLE )
         implicit none
         real(DbKi), intent(in) :: ANGLE
         real(DbKi)  PI

         PI = 3.14159265358979323846_DbKi
         COSD = DCOS ( ANGLE * PI / 180.0 )

         return
      end function COSD
!=======================================================================
      double precision function TAND( ANGLE )
         implicit none
         real(DbKi), intent(in) :: ANGLE
         real(DbKi)  PI

         PI = 3.14159265358979323846_DbKi
         TAND = DTAN ( ANGLE * PI / 180.0 )

         return
      end function TAND
!=======================================================================
      double precision function ATAND( ARG )
         implicit none
         real(DbKi), intent(in) :: ARG
         real(DbKi)  PI

         PI = 3.14159265358979323846_DbKi
         ATAND = DATAN ( ARG ) * 180 / PI

         return
      end function ATAND
!=======================================================================
      double precision function ASIND( ARG )
         implicit none
         real(DbKi), intent(in) :: ARG
         real(DbKi)  PI

         PI = 3.14159265358979323846_DbKi
         ASIND = DASIN ( ARG ) * 180 / PI

         return
      end function ASIND
!=======================================================================
      double precision function ACOSD( ARG )
         implicit none
         real(DbKi), intent(in) :: ARG
         real(DbKi)  PI

         PI = 3.14159265358979323846_DbKi
         ACOSD = DACOS ( ARG ) * 180 / PI

         return
      end function ACOSD

end module ParsMod

