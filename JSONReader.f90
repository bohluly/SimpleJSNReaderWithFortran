
!Developed by Bohluly@ut.ac.ir
!To Read JSN Data bases
!Simple subroutines to read JSON data bases on nested structured types variables.

!There is not any error control so JSON file must be valid.
!There is not any limitations for number of layers or objects or arrays of data base in program. 
!There is not any subroutines for export data.
!there are only Two Subroutine (readJSONFile) and (SplitLevelObjects) 
!SplitLevelObjects is a recursive Subroutine 
    
USE DataJSN
TYPE(ClassOBJSplit)             :: mainFileOBJ

call readJSONFile("f:\example_2.json",mainFileOBJ) 

call SplitLevelObjects(mainFileOBJ)

END

!=============================================================    
RECURSIVE Subroutine SplitLevelObjects(mainObJ)
USE DataJSN

INTEGER(8)   :: stringLen , is  , ist , nOBJ
CHARACTER(1) :: Char
CHARACTER(20) :: temp
Logical      :: StartObj  , StartName  
type(ClassOBJSplit) :: mainObJ
type(ClassOBJSplit),allocatable :: OBJ(:)

stringLen = len(mainObJ.Value)

StartObj = .false. 
StartName = .false. 
nOpen  = 0
nOBJ = 0

do is = 2 ,  stringLen-1
          Char = mainObJ.Value(is:is)  
      if (Char == '{' .or. Char == '[' ) nOpen = nOpen + 1
      if (Char == '}' .or. Char == ']' ) nOpen = nOpen - 1
      if (nOpen==0 .and. Char==',')      nOBJ = nOBJ + 1  
enddo     
Allocate (OBJ(nOBJ+1))

nOpen  = 0
nOBJ = 0

do is = 2 ,  stringLen-1
     Char = mainObJ.Value(is:is)  
 
      ! Control Objects and arrays
      if (Char == '{' .or. Char == '[' ) nOpen = nOpen + 1
      if (Char == '}' .or. Char == ']' ) nOpen = nOpen - 1
     
    if (StartObj) then 
      
      if (StartName) then
         if (Char ==":") then 
             StartName = .false.
             if (mainObJ.Value(is+1:is+1) == "[") OBJ(nOBJ).isArray = .true.
             CYCLE
         else if (Char =='"') then
             CYCLE 
         endif 
         OBJ(nOBJ).Name = OBJ(nOBJ).Name // Char 
         CYCLE    
      endif    
        
      ! read string of an object 

      if (nOpen > 0 ) OBJ(nOBJ).isSeparable = .true.
      
      if ((nOpen > 0 .or. Char /= ',')) then 
          OBJ(nOBJ).Value = OBJ(nOBJ).Value // Char
          Char=" "
      else 
          StartObj=.false. 
      endif

    else if (char == '"' .and. .not.(mainObJ.isArray)) then
         StartObj  =.true.
         StartName =.true.
         nOpen = 0
         nOBJ  = nOBJ + 1 
         OBJ(nOBJ).Name= ""
         OBJ(nOBJ).Value = ""
         CYCLE
    else  if (mainObJ.isArray) then
         StartObj  =.true.
         StartName = .false.
         nOpen = 0
         
         if (Char == '{') nOpen = nOpen + 1
         if (Char == '[') nOpen = nOpen + 1

         nOBJ  = nOBJ + 1 
         
         if  (nOBJ >= 10000) write (temp,"(A2,I6)") "i=",nOBJ
         if  (nOBJ <  10000) write (temp,"(A2,I4)") "i=",nOBJ
         if  (nOBJ <    100) write (temp,"(A2,I2)") "i=",nOBJ

         
         OBJ(nOBJ).Name= trim(temp)
         OBJ(nOBJ).Value = Char
         CYCLE
        
    endif      
enddo    

mainObJ.Value="-"
mainObJ.nOBJs = nOBJ
Allocate (mainObJ.OBJ(nOBJ)) 

DO iobj = 1,nOBJ
    mainObJ.OBJ(iobj).Name         = OBJ(iobj).Name
    mainObJ.OBJ(iobj).Value        = OBJ(iobj).Value
    mainObJ.OBJ(iobj).isSeparable  = OBJ(iobj).isSeparable
    mainObJ.OBJ(iobj).isArray      = OBJ(iobj).isArray
    if (mainObJ.OBJ(iobj).isSeparable) then 
        CALL SplitLevelObjects(mainObJ.OBJ(iobj))
    endif 
ENDDO      

    DeAllocate (OBJ)

End
    
    
!=============================================================    
Subroutine readJSONFile(JSNfilename,mainFileOBJ)
USE DataJSN

CHARACTER(*)     :: JSNfilename 
CHARACTER(len=1) :: buffer
Integer(1)       :: iQ
TYPE(ClassOBJSplit)             :: mainFileOBJ

!=============================================
OPEN(100 , file =  JSNfilename, STATUS='old', IOSTAT=ios)

IF ( ios /= 0 ) THEN
    stop "Error opening file JSN"
ENDIF 

!Read all lines of JSN file
iQ = -1                          
DO
        read(100,'(A1)', advance = "no", IOSTAT=ios) buffer
        IF (is_iostat_end(ios)) exit
        if (buffer == '"') iQ = -iQ
        if (buffer /= ' '.or. iQ == 1)                 mainFileOBJ.Value = mainFileOBJ.Value // buffer
        
ENDDO   

CLOSE (100)

mainFileOBJ.Name = '"'//JSNfilename//'"'
mainFileOBJ.isSeparable = .true.
End    
    
    
