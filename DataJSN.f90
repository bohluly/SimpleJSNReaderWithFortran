!Data Base    
Module DataJSN
    
    TYPE ClassOBJSplit
        CHARACTER(len=:) , allocatable  :: Value    
        CHARACTER(len=:) , allocatable  :: Name
        LOGICAL                         :: isSeparable = .false. , isArray = .false.
        INTEGER                         :: nOBJs
        type(ClassOBJSplit),allocatable :: OBJ(:)
    END TYPE ClassOBJSplit 

END Module 
    