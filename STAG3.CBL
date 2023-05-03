*                                                                       00000100
* MAP TP STAGIAIRE                                      00000100
*                                                                       00000100
        PRINT NOGEN
STAG3   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                    X00000100
               CTRL=(FREEKB,FRSET),STORAGE=AUTO,TIOAPFX=YES             00000200
MAP0    DFHMDI COLUMN=1,LINE=1,SIZE=(24,80),MAPATTS=(COLOR,HILIGHT),   X00000210
               DSATTS=(COLOR,HILIGHT)
TERM0   DFHMDF POS=(01,01),ATTRB=ASKIP,LENGTH=13,COLOR=NEUTRAL 
        DFHMDF POS=(01,29),HILIGHT=UNDERLINE,                          X00000211
               ATTRB=ASKIP,LENGTH=22,                                  X00000220
               INITIAL='GESTION DES STAGIAIRES',COLOR=BLUE
        DFHMDF POS=(01,53),ATTRB=ASKIP,LENGTH=0
DAT0    DFHMDF POS=(01,70),ATTRB=ASKIP,LENGTH=10
OPER0   DFHMDF POS=(02,01),ATTRB=ASKIP,LENGTH=14
TIM0    DFHMDF POS=(02,72),ATTRB=ASKIP,LENGTH=8
        DFHMDF POS=(04,11),                                            X00000211
               ATTRB=ASKIP,LENGTH=25,                                  X
               INITIAL='1-CREATION D''UN STAGIAIRE'
        DFHMDF POS=(06,11),                                            X00000211
               ATTRB=ASKIP,LENGTH=29,                                  X
               INITIAL='2-CONSULTATION D''UN STAGIAIRE'
        DFHMDF POS=(08,11),                                            X00000211
               ATTRB=ASKIP,LENGTH=28,                                  X
               INITIAL='3-SUPPRESSION D''UN STAGIAIRE'
        DFHMDF POS=(10,11),                                            X00000211
               ATTRB=ASKIP,LENGTH=29,                                  X
               INITIAL='4-MODIFICATION D''UN STAGIAIRE'
        DFHMDF POS=(12,11),                                            X00000211
               ATTRB=ASKIP,LENGTH=31,                                  X
               INITIAL='5-LISTE DE PLUSIEURS STAGIAIRES'
        DFHMDF POS=(14,20),                                            X00000211
               ATTRB=ASKIP,LENGTH=20,                                  X00000220
               INITIAL='TAPEZ VOTRE CHOIX : ' 
NUM0    DFHMDF POS=(14,41),COLOR=YELLOW,                               X00000400
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
        DFHMSD TYPE=FINAL                                               00000800
        END                                                             00000900
