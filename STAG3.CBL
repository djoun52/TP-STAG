*                                                                       00000100
* MAP TP STAGIAIRE                                      00000100
*                                                                       00000100
        PRINT NOGEN
STAG3   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                    X00000100
               CTRL=(FREEKB,FRSET),STORAGE=AUTO,TIOAPFX=YES             00000200
MAP0    DFHMDI COLUMN=1,LINE=1,SIZE=(24,80),MAPATTS=(COLOR,HILIGHT),   X00000210
               DSATTS=(COLOR,HILIGHT)
TERM0   DFHMDF POS=(01,01),ATTRB=ASKIP,LENGTH=13
        DFHMDF POS=(01,29),HILIGHT=UNDERLINE,                          X00000211
               ATTRB=ASKIP,LENGTH=23,                                  X00000220
               INITIAL='GESTION DES STAGIAIRE',COLOR=BLUE
        DFHMDF POS=(01,53),ATTRB=ASKIP,LENGTH=0
DAT0    DFHMDF POS=(01,70),ATTRB=ASKIP,LENGTH=10
OPER0   DFHMDF POS=(02,01),ATTRB=ASKIP,LENGTH=14
TIM0    DFHMDF POS=(02,72),ATTRB=ASKIP,LENGTH=8
        DFHMDF POS=(04,11),                                            X00000211
               ATTRB=ASKIP,LENGTH=25                                   X
               INITIAL='1-CREATION D''UN STAGIAIRE'
        DFHMDF POS=(06,11),                                            X00000211
               ATTRB=ASKIP,LENGTH=29                                   X
               INITIAL='2-CONSULTATION D''UN STAGIAIRE'
        DFHMDF POS=(06,11),                                            X00000211
               ATTRB=ASKIP,LENGTH=29                                   X
               INITIAL='2-CONSULTATION D''UN STAGIAIRE'
        DFHMDF POS=(06,11),                                            X00000211
               ATTRB=ASKIP,LENGTH=29                                   X
               INITIAL='2-CONSULTATION D''UN STAGIAIRE'
        DFHMDF POS=(14,15),                                            X00000211
               ATTRB=ASKIP,LENGTH=20,                                  X00000220
               INITIAL='TAPEZ VOTRE CHOIX : ' 
NUM0    DFHMDF POS=(14,33),COLOR=YELLOW,                               X00000400
               ATTRB=(UNPROT,NUM,IC,FSET),LENGTH=1                      00000410
        DFHMDF POS=(04,38),ATTRB=ASKIP,LENGTH=0
        DFHMDF POS=(15,61),ATTRB=ASKIP,LENGTH=0
MESS0   DFHMDF POS=(23,01),ATTRB=ASKIP,LENGTH=79,COLOR=RED
        DFHMDF POS=(24,01),ATTRB=ASKIP,LENGTH=79,                      X
               INITIAL='PF12=FIN         ENTER=VALIDATION'                                      
*                                1         2         3         4
*                       12345678901234567890123456789012345678901234567
*                5         6         7        7
*              89012345678901234567890123456789
MAP1    DFHMDI COLUMN=1,LINE=1,SIZE=(24,80),MAPATTS=(COLOR,HILIGHT),   X00000210
               DSATTS=(COLOR,HILIGHT)
TERM1   DFHMDF POS=(01,01),ATTRB=ASKIP,LENGTH=13
        DFHMDF POS=(01,29),                                            X00000211
               ATTRB=ASKIP,LENGTH=23,                                  X00000220
               INITIAL='BALAYAGE DU TEST.KSDS',COLOR=NEUTRAL            00000300
        DFHMDF POS=(01,53),ATTRB=ASKIP,LENGTH=0
DAT1    DFHMDF POS=(01,70),ATTRB=ASKIP,LENGTH=10
OPER1   DFHMDF POS=(02,01),ATTRB=ASKIP,LENGTH=14
TIM1    DFHMDF POS=(02,72),ATTRB=ASKIP,LENGTH=8
        DFHMDF POS=(03,60),ATTRB=ASKIP,LENGTH=4,INITIAL='PAGE'
PAG1    DFHMDF POS=(03,65),ATTRB=ASKIP,LENGTH=5         
        DFHMDF POS=(04,15),                                            X00000211
               ATTRB=ASKIP,LENGTH=17,COLOR=RED,                        X00000220
               INITIAL='NUMERO PRODUIT : '                             
NUM1    DFHMDF POS=(04,33),COLOR=YELLOW,                               X     
               ATTRB=(UNPROT,NUM,IC,FSET),LENGTH=4                      00000410
        DFHMDF POS=(04,38),ATTRB=ASKIP,LENGTH=0                     
        DFHMDF POS=(05,01),ATTRB=ASKIP,LENGTH=79,                      X 
               INITIAL='.----------------------------------------------X
               -------------------------------.' 
        DFHMDF POS=(06,01),ATTRB=ASKIP,LENGTH=79,                      X 
               INITIAL='|       NUMERO        |            INTITULE    X
                         |      CATEGORIE     |' 
        DFHMDF POS=(07,01),ATTRB=ASKIP,LENGTH=79,                      X 
               INITIAL='|---------------------|------------------------X
               ----------|--------------------|'                  
LIG1    DFHMDF POS=(08,01),ATTRB=(ASKIP,FSET),LENGTH=79,OCCURS=8  
        DFHMDF POS=(16,01),ATTRB=ASKIP,LENGTH=79,                      X 
               INITIAL='''---------------------------------------------X
               --------------------------------'''          
MESS1   DFHMDF POS=(23,01),ATTRB=ASKIP,LENGTH=79,COLOR=RED
        DFHMDF POS=(24,01),ATTRB=ASKIP,LENGTH=79,                      X
               INITIAL='ENTER=LECTURE    F3=RETOUR   F7=PAG-PREC F8=PAGX
               -SUIV    CLEAR=FIN'
MAP2    DFHMDI COLUMN=35,LINE=08,SIZE=(06,13),MAPATTS=(COLOR,HILIGHT), X
               DSATTS=(COLOR,HILIGHT)
        DFHMDF POS=(01,01),ATTRB=ASKIP,LENGTH=12,                      X
               INITIAL='.----------.'  
        DFHMDF POS=(02,01),ATTRB=ASKIP,LENGTH=12,                      X
               INITIAL='| 1 = VIEW |'
        DFHMDF POS=(03,01),ATTRB=ASKIP,LENGTH=12,                      X
               INITIAL='| 2 = UPDT |'
        DFHMDF POS=(04,01),ATTRB=ASKIP,LENGTH=12,                      X
               INITIAL='| 3 = DEL  |'
        DFHMDF POS=(05,01),ATTRB=ASKIP,LENGTH=08,                      X
               INITIAL='| OPTION'
OPT2    DFHMDF POS=(05,10),ATTRB=(UNPROT,NUM,FSET,IC),LENGTH=1,        X
               INITIAL='1'
        DFHMDF POS=(05,12),ATTRB=ASKIP,LENGTH=01,                      X
               INITIAL='|'
        DFHMDF POS=(06,01),ATTRB=ASKIP,LENGTH=12,                      X 
               INITIAL='''----------'''
        DFHMSD TYPE=FINAL                                               00000800
        END                                                             00000900
