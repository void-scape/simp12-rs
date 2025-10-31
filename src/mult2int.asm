	JMP ZERO_RESULT

ZERO_RESULT:
    LOAD E2     
    SUB E2  
    STORE FF

ADD_MULT_TO_RESULT:
    LOAD FE     
    JZ DONE    
    
    LOAD FF    
    ADD FD    
    STORE FF 
    
    LOAD FE 
    SUB 00  
    STORE FE 
    JMP ADD_MULT_TO_RESULT

DONE:
    HALT
