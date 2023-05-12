       IDENTIFICATION DIVISION.
       PROGRAM-ID. INF5CI3.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COMMAREA.
           05 PROG-PRECEDENT      PIC X(8).
           05 PROG-COURANT        PIC X(8).
           05 PROG-SUIVANT        PIC X(8).
           05 ETAPE               PIC X.
           05 FIN-FIC             PIC X.
           05 TS-NOM.
             10 TS-NOM1           PIC X(4).
             10 TS-NOM2           PIC X(4).
           05 ITEMCOUR            PIC S9(4) COMP. 
           05 ITEMMAXI            PIC S9(4) COMP.
           05 CA-NUMERO           PIC X(4). 
           05 CA-OPT              PIC X. 
           05 FILLER              PIC X(07).
       01 MAXI-ED                 PIC Z9.
       01 COUR-ED                 PIC Z9.
       01 POS                     PIC S9(8) COMP.
       01 C-R                     PIC S9(8) COMP.
       01 C-R-ED                  PIC ZZZ9.
       01 WS-TIME                 PIC X(8).
       01 TEXT-ERREUR             PIC X(15).
       01 MON-PROG                PIC X(8) VALUE 'INF5CI3 '.
       01 PROGMENU                PIC X(8) VALUE 'INF0CI3 '.
       01 SPERR                   PIC X(8) VALUE 'SPERR3  '.
       01 MON-FICHIER             PIC X(8) VALUE 'FSTAG03 '.
       01 MA-MAP                  PIC X(8) VALUE 'MAP5    '.
       01 MON-MAPSET              PIC X(8) VALUE 'MAPC3   '.
       01 MA-TRX                  PIC X(4) VALUE 'T5C3'.
       01 WS-NUMERO.             
           05 WS-NUMERO9          PIC 9999.
      *   DESCRIPTION DE LA MAP           
       COPY MAPC3.

      *   DESCRIPTION DU FICHIER KSD                       *
       COPY FSTAG.
      *   DESCRIPTION DE LA LIGNE DE LA GRILLE 
       01 LIGNE.
         05 VALUE SPACE          PIC X(1).
         05 LI-NUMERO            PIC X(4).
         05 VALUE SPACE          PIC X(2).
         05 VALUE '|'            PIC X.
         05 VALUE SPACE          PIC X(1).
         05 LI-NOM               PIC X(20).
         05 VALUE '|'            PIC X.
         05 VALUE SPACE          PIC X(1).
         05 LI-PRENOM            PIC X(20).
         05 VALUE '|'            PIC X.   
         05 VALUE SPACE          PIC X(1).
         05 LI-DATE-NAISS         PIC X(8).
         05 VALUE SPACE          PIC X(1).
         05 VALUE '|'            PIC X.  
       77  IND-LIG               PIC S9(4) COMP.    
       01 LIGNE-VIDE.
         05 VALUE SPACE          PIC X(1).
         05 VALUE SPACE          PIC X(4).
         05 VALUE SPACE          PIC X(2).
         05 VALUE '|'            PIC X.
         05 VALUE SPACE          PIC X(1).
         05 VALUE SPACE          PIC X(20).
         05 VALUE '|'            PIC X.
         05 VALUE SPACE          PIC X(1).
         05 VALUE SPACE          PIC X(20).
         05 VALUE '|'            PIC X.   
         05 VALUE SPACE          PIC X(1).
         05 VALUE SPACE          PIC X(8).
         05 VALUE SPACE          PIC X(1).
         05 VALUE '|'            PIC X.  

      *   ZONE DE MESSAGE TAMPON POUR LE SEND FROM           *
       01  MESSAGE-TXT            PIC X(79).

      *   DESCRIPTION   DES  TOUCHES   FONCTIONS             *
       COPY DFHAID.

      *   DESCRIPTION   DES  ATTRIBUTS                       *
       COPY DFHBMSCA.


       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05 LK-COMMAREA            PIC X(50).
       PROCEDURE DIVISION.
       00000-INIT-PROGRAMME.
      *---------------------*
            PERFORM  10000-DEBUT-PROGRAMME
            PERFORM  20000-TRAIT-PROGRAMME
            GOBACK.


       10000-DEBUT-PROGRAMME.
      *---------------------*
      *   TEST DE PREMIERE ENTREE DANS LE PROGRAMME
      *   SI NON PROGRAMME  MENU ===>  ARRET  (ACCES INTERDIT)
           IF EIBCALEN = 0
              INITIALIZE      WS-COMMAREA
           ELSE
              MOVE LK-COMMAREA  TO WS-COMMAREA
           END-IF

           MOVE  PROG-COURANT     TO  PROG-PRECEDENT
           MOVE  MON-PROG         TO  PROG-COURANT
           .

       20000-TRAIT-PROGRAMME.
      *---------------------*

           IF PROG-PRECEDENT  NOT =  PROG-COURANT
              PERFORM  21000-TRAIT-CHARGEMENT
              PERFORM  22000-TRAIT-ENVOI
           ELSE
              PERFORM  23000-TRAIT-RECEPTION
           END-IF
           .
      *********************************************************

       21000-TRAIT-CHARGEMENT.
      *----------------------*
        
      *  CHARGEMENT DE LA MAP AVANT AFFICHAGE
           MOVE LOW-VALUE TO MAP5I
           MOVE EIBTRMID  TO TS-NOM1
           MOVE MA-TRX    TO TS-NOM2
           PERFORM 40000-DELETE-TS
      *  SI TRAITEMENT PARTICULIER AVANT AFFICHAGE
           PERFORM  21100-TRAIT-SPECIFIQUE
           .
       21100-TRAIT-SPECIFIQUE.
      *----------------------*
      *    ..............................
      *    ..............................
      *    .............................. 
      *    ..............................
           EXIT
           .


       22000-TRAIT-ENVOI.
      *-----------------*
           STRING EIBTRMID '/' MON-MAPSET DELIMITED BY SIZE
                  INTO TERM5O
           MOVE ITEMCOUR TO COUR-ED     
           MOVE ITEMMAXI TO MAXI-ED 
           STRING COUR-ED '/' MAXI-ED DELIMITED BY SIZE INTO PAG5O
           EXEC CICS ASKTIME ABSTIME(WS-TIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-TIME)
                               DDMMYYYY(DAT5O)
                               DATESEP ('/')
                               TIME (TIM5O)
                               TIMESEP(':')
           END-EXEC
           EXEC CICS SEND MAP    (MA-MAP)
                          MAPSET (MON-MAPSET)
                          FROM   (MAP5O)
                          ERASE
           END-EXEC

           MOVE PROG-COURANT TO PROG-SUIVANT

           EXEC CICS RETURN TRANSID  (MA-TRX)
                            COMMAREA (WS-COMMAREA)
                            LENGTH   (LENGTH OF WS-COMMAREA)
           END-EXEC
           .

       23000-TRAIT-RECEPTION.
      *---------------------*
           EVALUATE EIBAID
              WHEN DFHENTER
              WHEN DFHPF7
              WHEN DFHPF8
                   PERFORM 23100-TRAIT-ENTER    
              WHEN DFHPF3
                   PERFORM  40000-DELETE-TS
                   MOVE PROGMENU TO PROG-SUIVANT
                   PERFORM 23050-PROG-SUIVANT 
              WHEN DFHPF12
                   MOVE ' ' TO ETAPE
                   PERFORM 40000-DELETE-TS
                   PERFORM 23200-TRAIT-FIN  
              WHEN OTHER
                   PERFORM  90000-ERR-TOUCHE
           END-EVALUATE
           .

       23050-PROG-SUIVANT.
      *------------------*

           EXEC CICS XCTL  PROGRAM  (PROG-SUIVANT)
                           COMMAREA (WS-COMMAREA)
                           LENGTH   (LENGTH OF WS-COMMAREA)
                           RESP     (C-R)
           END-EXEC
           STRING 'PROGRAMME ' PROG-SUIVANT ' NON DISPONIBLE'
                  DELIMITED BY SIZE INTO MESS5O
           PERFORM 22000-TRAIT-ENVOI
           .
       23100-TRAIT-ENTER.
      *------------------*
           EXEC CICS RECEIVE MAP   (MA-MAP)
                             MAPSET(MON-MAPSET)
                             INTO  (MAP5I)
                             RESP  (C-R)
           END-EXEC
           EVALUATE C-R
              WHEN DFHRESP(NORMAL)
                CONTINUE
              WHEN DFHRESP(MAPFAIL)
                CONTINUE
              WHEN OTHER
                PERFORM 91000-ERREUR-CICS
           END-EVALUATE
           EVALUATE EIBAID
             WHEN DFHPF7
               PERFORM 23600-PAGE-PRECEDENTE
             WHEN DFHPF8
               PERFORM 23700-PAGE-SUIVANTE
           END-EVALUATE
           DIVIDE EIBCPOSN BY 80 GIVING POS
             PERFORM 40000-DELETE-TS
           PERFORM 23400-LECTURE
           PERFORM 22000-TRAIT-ENVOI
           .
       23200-TRAIT-FIN.
      *---------------*
           MOVE 'FIN DE LA TRANSACTION' TO MESSAGE-TXT
           PERFORM   99000-FIN-CICS.

       23400-LECTURE.
      *-------------*
           EXEC CICS STARTBR FILE (MON-FICHIER)
                              RIDFLD (NUM5I)
                              GTEQ
                              RESP   (C-R)
           END-EXEC
           IF C-R  = DFHRESP(NOTFND)
             MOVE 'NUMERO DE STAGIAIRE INVALIDE' TO MESS5O
             PERFORM 22000-TRAIT-ENVOI
           END-IF
           IF C-R NOT = DFHRESP(NORMAL)
             PERFORM 80000-APPEL-SPERR
             STRING 'ERREUR STARTBR ' TEXT-ERREUR
                    DELIMITED BY SIZE INTO MESS5O
             PERFORM 22000-TRAIT-ENVOI
           END-IF
           MOVE NUM5I TO E-NUMERO
           EXEC CICS READNEXT FILE   (MON-FICHIER)
                              RIDFLD (E-NUMERO)
                              INTO   (E-STAGIAIRE)
                              RESP   (C-R)
           END-EXEC
           PERFORM VARYING IND-LIG FROM 1 BY 1 UNTIL IND-LIG > 10
             EVALUATE C-R
               WHEN DFHRESP(NORMAL)
                 MOVE E-NUMERO       TO LI-NUMERO
                 MOVE E-NOM          TO LI-NOM
                 MOVE E-PRENOM       TO LI-PRENOM 
                 MOVE E-DATE-NAISS   TO LI-DATE-NAISS 
                 MOVE LIGNE          TO LIG5O(IND-LIG)
               WHEN DFHRESP(ENDFILE)
                 MOVE 'Y' TO FIN-FIC
                 PERFORM VARYING IND-LIG FROM IND-LIG
                         BY 1 UNTIL IND-LIG > 10
                   MOVE LIGNE-VIDE TO LIG5O(IND-LIG)
                 END-PERFORM
               WHEN OTHER
                 PERFORM 80000-APPEL-SPERR
                 STRING 'ERREUR FICHIER ' TEXT-ERREUR 
                        DELIMITED BY SIZE INTO MESS5O
             END-EVALUATE
             EXEC CICS READNEXT FILE   (MON-FICHIER)
                                RIDFLD (E-NUMERO)
                                INTO   (E-STAGIAIRE)
                                RESP   (C-R)
             END-EXEC
           END-PERFORM
           IF LIG5O(1) NOT = LIGNE-VIDE
             EXEC CICS WRITEQ TS QUEUE(TS-NOM)
                                 FROM (MAP5O)
                                 ITEM (ITEMCOUR)
                                 RESP (C-R)
             END-EXEC
             IF C-R NOT = DFHRESP(NORMAL)
               PERFORM 80000-APPEL-SPERR              
               STRING 'ERREUR WRITEQ TS ' TEXT-ERREUR
                      DELIMITED BY SIZE INTO MESS5O
               PERFORM 22000-TRAIT-ENVOI
             END-IF
             MOVE ITEMCOUR TO ITEMMAXI
           ELSE
             PERFORM 41000-READQ-TS
           END-IF
           .
       23600-PAGE-PRECEDENTE.
      *----------------------*
           IF ITEMCOUR < 2 
             MOVE 'DEBUT DE LISTE ATEINT' TO MESS5O
           ELSE
             SUBTRACT 1 FROM ITEMCOUR
             PERFORM 41000-READQ-TS
           END-IF
           PERFORM 22000-TRAIT-ENVOI
           .
       23700-PAGE-SUIVANTE.
      *--------------------*
           IF ITEMCOUR < ITEMMAXI
             ADD 1 TO ITEMCOUR
             PERFORM 41000-READQ-TS
           ELSE 
             IF FIN-FIC = 'Y'
               MOVE 'FIN DE LISTE ATTEINTE' TO MESS5O
             ELSE
               MOVE LIG5I(8) TO LIGNE
               MOVE LI-NUMERO TO WS-NUMERO
               ADD 1 TO WS-NUMERO9
               MOVE WS-NUMERO TO NUM5I
               PERFORM 23400-LECTURE
             END-IF
           END-IF
           PERFORM 22000-TRAIT-ENVOI 
           .
       

       40000-DELETE-TS.
      *----------------*
           EXEC CICS DELETEQ TS QUEUE(TS-NOM) RESP(C-R) END-EXEC
           MOVE 'N' TO FIN-FIC
           MOVE 0 TO ITEMMAXI ITEMCOUR
           .

       41000-READQ-TS.
      *----------------*
           EXEC CICS READQ TS QUEUE(TS-NOM) 
                              ITEM (ITEMCOUR)
                              INTO (MAP5O)
                              RESP(C-R) 
           END-EXEC
           IF C-R NOT = DFHRESP(NORMAL)
             PERFORM 80000-APPEL-SPERR
             STRING 'ERREUR READQ TS ' TEXT-ERREUR
                    DELIMITED BY SIZE INTO MESS5O
             PERFORM 22000-TRAIT-ENVOI
           END-IF
           .
       80000-APPEL-SPERR.
      *------------------*
           EXEC CICS LINK PROGRAM (SPERR)
                          INPUTMSG(C-R)
                          RESP    (C-R)
           END-EXEC
           IF C-R NOT = DFHRESP(NORMAL)
             MOVE C-R TO C-R-ED
             STRING 'ERREUR APPEL SPERR ' C-R-ED DELIMITED BY SIZE
                    INTO MESS5O
             PERFORM 22000-TRAIT-ENVOI
           END-IF
           EXEC CICS RECEIVE INTO(TEXT-ERREUR) RESP(C-R) END-EXEC
           IF C-R NOT = DFHRESP(NORMAL)
             MOVE C-R TO C-R-ED
             STRING 'ERREUR RECEIVE SPERR ' C-R-ED DELIMITED BY SIZE
                    INTO MESS5O
             PERFORM 22000-TRAIT-ENVOI
           END-IF
           .
       90000-ERR-TOUCHE.
      *----------------*
           MOVE 'TOUCHE DE FONCTION INVALIDE' TO MESS5O
           PERFORM  22000-TRAIT-ENVOI.


       91000-ERREUR-CICS.
      *-----------------*
           MOVE 'ERREUR CICS !!!  FIN DE LA TRANSACTION' TO MESSAGE-TXT
           PERFORM   99000-FIN-CICS.

       99000-FIN-CICS.
      *--------------*
           EXEC CICS SEND FROM   (MESSAGE-TXT)
                          LENGTH (LENGTH OF MESSAGE-TXT)
                          ERASE
           END-EXEC
           .
