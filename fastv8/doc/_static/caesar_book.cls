\NeedsTeXFormat{LaTeX2e}
\ProvidesFile{caesar_book.cls}[2016/04/21 v1.00 Caesar book]
% Load the amsmath and the mhchem package before loading microtype
\RequirePackage{amsmath}% better math handling
\RequirePackage[version=3]{mhchem}% chemical formulas
\LoadClassWithOptions{book}% this is derived from the book class
\RequirePackage{sidenotes}% put annotations, tables and figures in the margin
\captionsetup{font=footnotesize, skip=4pt}
\RequirePackage[maxfloats=45]{morefloats}% a lot of marginals need more float 'memory'
\RequirePackage{marginfix}% automatically adjust the side-floats nicely 
\newcommand{\caesar@letterspace}[1]{#1}% allow LuaLaTeX, LaTeX letterspacing
\RequirePackage{microtype}% microtypography
\@ifpackagelater{microtype}{2011/08/18}% xxx allow LaTeX and earlier microtype
{\renewcommand{\caesar@letterspace}[1]{\textls{#1}}}%
{\AtEndDocument{\ClassWarningNoLine{caesar_book}{Need newest microtype for letterspacing.}}}
%
% Set page layout geometry -- default is science-book format
% this means a (portrait) paper size of 170mm x 240mm
%
\RequirePackage{geometry}
\geometry{paperwidth=170mm, paperheight=240mm, left=42pt, top=40pt, textwidth=280pt, marginparsep=20pt, marginparwidth=100pt, textheight=560pt, footskip=40pt}
%
% fontsize is 10pt with a leading (line spacing) of 13pt for book 
%
\renewcommand{\normalsize}{\fontsize{10pt}{13pt}\selectfont}%
\renewcommand{\footnotesize}{\fontsize{8pt}{10pt}\selectfont}%
% fullwidth environment, text across textwidth+marginparsep+marginparwidth
\newlength{\overhang}
\setlength{\overhang}{\marginparwidth}
\addtolength{\overhang}{\marginparsep}
%
\newenvironment{fullwidth}
  {\ifthenelse{\boolean{@twoside}}%
     {\begin{adjustwidth*}{}{-\overhang}}%
     {\begin{adjustwidth}{}{-\overhang}}%
  }%
  {\ifthenelse{\boolean{@twoside}}%
    {\end{adjustwidth*}}%
    {\end{adjustwidth}}%
  }
% set up the fonts
\RequirePackage{ifluatex}
\ifluatex% in LuaLateX we use fontspec
	\RequirePackage{fontspec}%
	\RequirePackage[vargreek-shape=TeX]{unicode-math}% 
	\setmathfont{Asana-Math.otf}% asana math works well with palatino
	\setmainfont[Ligatures=TeX, Numbers=OldStyle]{Equity Text B}% free Palatino clone
	\setmonofont{Triplicate T3c}% works okay with palatino
	\setsansfont{Concourse T3}% work okay with palatino
	\mhchemoptions{textfontcommand=\addfontfeature{Numbers=Lining}} % use lining numbers for chemical formulas
        \newfontfamily\advocateCfiftyThree[]{Advocate C53}
        \newfontfamily\concourseTthree[]{Concourse T3}
        \newfontfamily\concourseCthree[]{Concourse C3}
        \newfontfamily\equityCapsB[]{Equity Caps B}
\else% in LaTeX we load fonts differently
	\IfFileExists{mathpazo.sty}{\RequirePackage[osf,sc]{mathpazo}}{}% palatino clone for math
  	\IfFileExists{helvet.sty}{\RequirePackage[scaled=0.90]{helvet}}{}% helvetica is sans serif font
  	\IfFileExists{beramono.sty}{\RequirePackage[scaled=0.85]{beramono}}{}% beramono is tt font
  	\RequirePackage[T1]{fontenc}% finally load fontenc
\fi
% nice headings and TOC
\RequirePackage{titlesec,titletoc}% load the helper package
%\setcounter{secnumdepth}{-1}% no numbering at all
\titleformat{\chapter}[hang]{\advocateCfiftyThree\huge}{\thechapter}{5pt}{\MakeUppercase}[]% chapter format

\titleformat{\section}[hang]{\concourseTthree\large\hyphenchar\font=-1}{\thesection}{5pt}{}[]% section format
\titlespacing{\section}{0pt}{10pt}{2pt}[-1in]
\titleformat{\subsection}[hang]{\normalfont\large\itshape}{}{1em}{}[]% subsection format
\titleformat{\paragraph}[runin]{\normalfont\itshape}{}{1em}{}[]% paragraph format
\renewcommand*\descriptionlabel[1]{\hspace\labelsep\normalfont\em #1}% italic description instead of bold)
\setcounter{tocdepth}{0}% The Table of Contents, only chapters 
\titlecontents{chapter}%
   [0em] % distance from left margin
   {\vspace{1.5\baselineskip}\begin{fullwidth}\large\concourseCthree}% above (global formatting of entry)
   {\contentslabel {1em}}% before w/label (label = ``2'')
   {\hspace*{-0.5em}} % before w/o label 
   {\hfill\thecontentspage} % filler + page (leaders and page num)
   [\end{fullwidth}] % after
%set up header, footer and page numbering
\RequirePackage{fancyhdr}% helper package
\renewcommand{\headrulewidth}{0pt}% The running heads and feet do not have rules
\renewcommand{\footrulewidth}{0pt}
\fancyhf{}% set up the page numbering 
{\fancyfoot[LE,RO]{\thepage}}
\fancypagestyle{plain}{%
	\fancyhf{}
}
\renewcommand{\frontmatter}{\pagestyle{empty}}% the frontmatter is empty
\renewcommand{\mainmatter}{\cleardoublepage\pagestyle{fancy}}% mainmatter with page numbering
\RequirePackage{ragged2e} % \RaggedRight works better than \raggedright
\newcommand{\marginparstyle}{\footnotesize\RaggedRight} % initialize style with start value
\long\def\@ympar#1{% redefine margin par to avoid too many macros in the document
  \@savemarbox\@marbox{\marginparstyle#1}% marginparstyle is a prefix to the marginpar text now
  \global\setbox\@currbox\copy\@marbox % the rest of the definition is taken from original LaTeX
  \@xympar}
\renewcommand*{\marginfont}{\marginparstyle}
% setup itemize and enumerate  
\RequirePackage{enumitem}
\setlist[itemize]{leftmargin=*}
%\renewcommand{\labelitemi}{}
% compatibility with T-L
%
% titlepage
\newcommand{\publisher}[1]{\gdef\caesar@publisher{#1}}
\gdef\caesar@thesis{}
\newcommand{\thesis}[1]{\gdef\caesar@thesis{#1}}
\RequirePackage{textcase} % provides \MakeTextUppercase and \MakeTextLowercase
\RequirePackage{color} % the title is too black, define gray
\definecolor{darkgray}{rgb}{0.25,0.25,0.25}
\newcommand{\maketitlepage}{% the title page is generated here
  \Huge\advocateCfiftyThree\MakeUppercase{\@title}
  \vspace{0.25em}%
  \section*{\caesar@thesis\@author, \caesar@publisher}
  \vspace{3em}%
  \normalsize\rmfamily
The aim of this project is to develop an light weight driver program for FAST v8 \phantomsection\label{index:id1}{\hyperref[zrefs:fastv8modframework]{\crossref{{[}Jon13{]}}}}  in C++. This program will serve as an example for people who want to interface FAST v8 with their own simulation framework. The broad requirements of the light weight driver program for FAST v8 are:
\begin{enumerate}
\item {} 
the program should be very similar to how it would be used in NALU \phantomsection\label{index:id2}{\hyperref[zrefs:naludoc]{\crossref{{[}Dom15{]}}}} .

\item {} 
the program should be capable of having a time step that is independent of FAST's time step.

\item {} 
the program should have an option to turn off the structural modes in FAST.

\item {} 
the program should be capable of instantiating multiple turbines inside FAST on the same processor core.

\item {} 
the program should have a restart capability.

\item {} 
the design of the program should allow a super controller to be built around all the turbines.

\end{enumerate}

  % 
}
\newcommand\newthought[1]{%
   \addvspace{1.0\baselineskip plus 0.5ex minus 0.2ex}%
   \noindent\textsc{#1}%
}%
\ExplSyntaxOn
\NewDocumentCommand \sidecite { o o m }
{ 
  \IfNoValueOrEmptyTF{#1}
  {
    \IfNoValueOrEmptyTF{#2}
    {
      \sidenote{\fullcite{#3}}
    }
    {
      \sidenote{\fullcite[#2]{#3}}
    }
  }
  {
    \IfNoValueOrEmptyTF{#2}
    {
      \sidenote{\fullcite[#1]{#3}}
    }
    {
      \sidenote{\fullcite[#1][#2]{#3}}
    }
  }
}
\ExplSyntaxOff


