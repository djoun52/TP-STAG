*                                                                       00000100
* MAP TP STAGIAIRE                                      
*                                                                       00000100
        PRINT NOGEN
STAG3   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                    X00000100
               CTRL=(FREEKB,FRSET),STORAGE=AUTO,TIOAPFX=YES             00000200
MAP0    DFHMDI COLUMN=1,LINE=1,SIZE=(24,80),MAPATTS=(COLOR,HILIGHT),   X00000210
               DSATTS=(COLOR,HILIGHT)
TERM0   DFHMDF POS=(01,01),ATTRB=ASKIP,LENGTH=13,COLOR=NEUTRAL 
        DFHMDF POS=(01,27),HILIGHT=UNDERLINE,                          X
               ATTRB=ASKIP,LENGTH=22,                                  X00000220
               INITIAL='GESTION DES STAGIAIRES',COLOR=BLUE
        DFHMDF POS=(01,50),ATTRB=ASKIP,LENGTH=0
DAT0    DFHMDF POS=(01,70),ATTRB=ASKIP,LENGTH=10
OPER0   DFHMDF POS=(02,01),ATTRB=ASKIP,LENGTH=14
TIM0    DFHMDF POS=(02,72),ATTRB=ASKIP,LENGTH=8
        DFHMDF POS=(04,10),                                            X
               ATTRB=ASKIP,LENGTH=25,                                  X
               INITIAL='1-CREATION D''UN STAGIAIRE'
        DFHMDF POS=(06,10),                                            X
               ATTRB=ASKIP,LENGTH=29,                                  X
               INITIAL='2-CONSULTATION D''UN STAGIAIRE'
        DFHMDF POS=(08,10),                                            X
               ATTRB=ASKIP,LENGTH=28,                                  X
               INITIAL='3-SUPPRESSION D''UN STAGIAIRE'
        DFHMDF POS=(10,10),                                            X
               ATTRB=ASKIP,LENGTH=29,                                  X
               INITIAL='4-MODIFICATION D''UN STAGIAIRE'
        DFHMDF POS=(12,10),                                            X
               ATTRB=ASKIP,LENGTH=31,                                  X
               INITIAL='5-LISTE DE PLUSIEURS STAGIAIRES'
        DFHMDF POS=(14,20),                                            X
               ATTRB=ASKIP,LENGTH=20,                                  X00000220
               INITIAL='TAPEZ VOTRE CHOIX : ' 
NUM0    DFHMDF POS=(14,41),COLOR=YELLOW,                               X00000400
               ATTRB=(UNPROT,NUM,IC,FSET),LENGTH=1                      00000410
        DFHMDF POS=(14,43),ATTRB=ASKIP,LENGTH=0
        DFHMDF POS=(15,61),ATTRB=ASKIP,LENGTH=0
        DFHMDF POS=(24,01),ATTRB=ASKIP,LENGTH=37,                      X
               INITIAL='PF12 - FIN         ENTER - VALIDATION'
MAP1    DFHMDI COLUMN=1,LINE=1,SIZE=(24,80),MAPATTS=(COLOR,HILIGHT),   X00000210
               DSATTS=(COLOR,HILIGHT)
TERM1   DFHMDF POS=(01,01),ATTRB=ASKIP,LENGTH=13
TTR0    DFHMDF POS=(01,27),ATTRB=ASKIP,LENGTH=27,                      X
               HILIGHT=UNDERLINE,COLOR=BLUE,                           X
               INITIAL='CREATION D''UN STAGIAIRE'
        DFHMDF POS=(01,51),ATTRB=ASKIP,LENGTH=0
DAT1    DFHMDF POS=(01,70),ATTRB=ASKIP,LENGTH=10
OPER1   DFHMDF POS=(02,01),ATTRB=ASKIP,LENGTH=14
TIM1    DFHMDF POS=(02,72),ATTRB=ASKIP,LENGTH=8
        DFHMDF POS=(04,10),ATTRB=ASKIP,LENGTH=21,                      X
               INITIAL='NUMERO DE STAGIAIRE*:'
NUM1    DFHMDF POS=(04,32),HILIGHT=UNDERLINE,                          X00000400
               ATTRB=(UNPROT,NUM,IC,FSET),LENGTH=4   
        DFHMDF POS=(04,37),ATTRB=ASKIP,LENGTH=0       
        DFHMDF POS=(06,10),ATTRB=ASKIP,LENGTH=5,                       X00000211
               INITIAL='NOM*:'
NOM0    DFHMDF POS=(06,16),HILIGHT=UNDERLINE,                          X00000400
               ATTRB=(UNPROT,IC,FSET),LENGTH=25   
        DFHMDF POS=(06,42),ATTRB=ASKIP,LENGTH=0       
        DFHMDF POS=(06,43),ATTRB=ASKIP,LENGTH=8,                       X
               INITIAL='PRENOM*:'
PNM0    DFHMDF POS=(06,52),HILIGHT=UNDERLINE,                          X00000400
               ATTRB=(UNPROT,IC,FSET),LENGTH=25
        DFHMDF POS=(06,78),ATTRB=ASKIP,LENGTH=0          
        DFHMDF POS=(08,10),ATTRB=ASKIP,LENGTH=11,                      X
               INITIAL='ADRESSE 1 :'
ADS0    DFHMDF POS=(08,22),HILIGHT=UNDERLINE,                          X00000400
               ATTRB=(UNPROT,IC,FSET),LENGTH=30
        DFHMDF POS=(08,53),ATTRB=ASKIP,LENGTH=0   
        DFHMDF POS=(10,15),ATTRB=ASKIP,LENGTH=13,                      X
               INITIAL='CODE POSTAL :'
CP0     DFHMDF POS=(10,29),HILIGHT=UNDERLINE,                          X00000400
               ATTRB=(UNPROT,IC,FSET),LENGTH=5
        DFHMDF POS=(10,35),ATTRB=ASKIP,LENGTH=0   
        DFHMDF POS=(10,38),ATTRB=ASKIP,LENGTH=7,                       X
               INITIAL='VILLE :'
VL0     DFHMDF POS=(10,46),HILIGHT=UNDERLINE,                          X00000400
               ATTRB=(UNPROT,IC,FSET),LENGTH=30
        DFHMDF POS=(10,77),ATTRB=ASKIP,LENGTH=0   
        DFHMDF POS=(14,10),ATTRB=ASKIP,LENGTH=16,                      X
               INITIAL='TELEPHONE FIXE :'
TF0     DFHMDF POS=(14,27),HILIGHT=UNDERLINE,                          X00000400
               ATTRB=(UNPROT,NUM,IC,FSET),LENGTH=10
        DFHMDF POS=(14,38),ATTRB=ASKIP,LENGTH=0   
        DFHMDF POS=(14,40),ATTRB=ASKIP,LENGTH=18,                      X
               INITIAL='TELEPHONE MOBILE :'
TM0     DFHMDF POS=(14,59),HILIGHT=UNDERLINE,                          X00000400
               ATTRB=(UNPROT,NUM,IC,FSET),LENGTH=10
        DFHMDF POS=(14,70),ATTRB=ASKIP,LENGTH=0   
        DFHMDF POS=(16,10),ATTRB=ASKIP,LENGTH=30,                      X
               INITIAL='DATE DE NAISSANCE (JJMMAAAA)*:'
DDN0    DFHMDF POS=(16,41),HILIGHT=UNDERLINE,                          X00000400
               ATTRB=(UNPROT,NUM,IC,FSET),LENGTH=8
        DFHMDF POS=(16,50),ATTRB=ASKIP,LENGTH=0
LCONF0  DFHMDF POS=(18,25),ATTRB=(ASKIP,DRK),LENGTH=33,                X00000400
               INITIAL='EST-CE QUE VOUS CONFIRMEZ ?(Y/N)'               00000300
CONF0   DFHMDF POS=(18,59),ATTRB=(ASKIP,DRK,FSET),LENGTH=1,INITIAL='N'  00000400
        DFHMDF POS=(18,61),ATTRB=ASKIP,LENGTH=0            
MESS0   DFHMDF POS=(22,01),ATTRB=ASKIP,LENGTH=79,COLOR=RED        
MESS1   DFHMDF POS=(23,01),ATTRB=ASKIP,LENGTH=79,COLOR=RED,            X
               INITIAL='----------------------------------------'
        DFHMDF POS=(24,01),ATTRB=ASKIP,LENGTH=54,                      X
               INITIAL='PF12 - FIN       PF3 - RETOUR       ENTER - VALX
               IDATION' 
MAP2    DFHMDI COLUMN=1,LINE=1,SIZE=(24,80),MAPATTS=(COLOR,HILIGHT),   X00000210
               DSATTS=(COLOR,HILIGHT)
TERM2   DFHMDF POS=(01,01),ATTRB=ASKIP,LENGTH=13
        DFHMDF POS=(01,27),HILIGHT=UNDERLINE,                          X00000211
               ATTRB=ASKIP,LENGTH=19,                                  X00000220
               INITIAL='LISTE DES STAGAIRES',COLOR=BLUE         
        DFHMDF POS=(01,47),ATTRB=ASKIP,LENGTH=0
DAT2    DFHMDF POS=(01,70),ATTRB=ASKIP,LENGTH=10
OPER2   DFHMDF POS=(02,01),ATTRB=ASKIP,LENGTH=14
TIM2    DFHMDF POS=(02,72),ATTRB=ASKIP,LENGTH=8
        DFHMDF POS=(04,10),ATTRB=ASKIP,LENGTH=35,                      X
               INITIAL='N° DE STAGIAIRE DU DEBUT DE LISTING'
NUM2    DFHMDF POS=(04,47),HILIGHT=UNDERLINE,                          X00000400
               ATTRB=(UNPROT,NUM,IC,FSET),LENGTH=4   
        DFHMDF POS=(04,52),ATTRB=ASKIP,LENGTH=0           
        DFHMDF POS=(04,60),ATTRB=ASKIP,LENGTH=6,INITIAL='PAGE :'
PAG1    DFHMDF POS=(04,67),ATTRB=ASKIP,LENGTH=4    
        DFHMDF POS=(06,02),ATTRB=ASKIP,LENGTH=65,                      X00000211
               INITIAL='IDENT  |         NOM         |        PRENOM   X
                     |  NE LE   |'
LIG1    DFHMDF POS=(07,01),ATTRB=(ASKIP,FSET),LENGTH=79,OCCURS=10                      
MESS2   DFHMDF POS=(22,01),ATTRB=ASKIP,LENGTH=79,COLOR=RED        
        DFHMDF POS=(23,01),ATTRB=ASKIP,LENGTH=79,COLOR=RED,            X
               INITIAL='-----------------------------------------------X
               --------------------------------'
        DFHMDF POS=(24,01),ATTRB=ASKIP,LENGTH=76,                      X
               INITIAL='PF12:FIN  PF3:RETOUR PF7-PF8:PRECEDENT-SUIVANT X
               ENTER:VALIDATION PF10:NOUVEAU'   
*                                1         2         3         4
*                       12345678901234567890123456789012345678901234567
*                5         6         7        7
*              89012345678901234567890123456789                          
        DFHMSD TYPE=FINAL                                               00000800
        END                                                             00000900