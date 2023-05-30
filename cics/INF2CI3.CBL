       IDENTIFICATION DIVISION.
       PROGRAM-ID. INF2CI3.
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
           05 MESSPROG            PIC X(40) .
           05 TS-NOM.
             10 TS-NOM1           PIC X(4).
             10 TS-NOM2           PIC X(4).
           05 FILLER              PIC X(26).
       01 C-R                     PIC S9(8) COMP.
       01 C-R-ED                  PIC ZZZ9.
       01 WS-TIME                 PIC X(8).
       01 TEXT-ERREUR             PIC X(15).
       01 MON-PROG                PIC X(8) VALUE 'INF2CI3 '.
       01 PROGMENU                PIC X(8) VALUE 'INF0CI3 '.
       01 SPERR                   PIC X(8) VALUE 'SPERR3  '.
       01 MON-FICHIER             PIC X(8) VALUE 'FSTAG03 '.
       01 MA-MAP                  PIC X(8) VALUE 'MAP2    '.
       01 MON-MAPSET              PIC X(8) VALUE 'MAPC3   '.
       01 MA-TRX                  PIC X(4) VALUE 'T2C3'.
       01 WS-NUMERO.
           05 WS-NUMERO9          PIC 9999.
      *   DESCRIPTION DE LA MAP
       COPY MAPC3.

      *   DESCRIPTION DU FICHIER KSD                         *
       COPY FSTAG.
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
           MOVE LOW-VALUE TO MAP2I
           MOVE EIBTRMID  TO TS-NOM1
           MOVE MA-TRX    TO TS-NOM2
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
                  INTO TERM2O
           EXEC CICS ASKTIME ABSTIME(WS-TIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-TIME)
                               DDMMYYYY(DAT2O)
                               DATESEP ('/')
                               TIME (TIM2O)
                               TIMESEP(':')
           END-EXEC
           EXEC CICS SEND MAP    (MA-MAP)
                          MAPSET (MON-MAPSET)
                          FROM   (MAP2O)
                          ERASE
                          CURSOR
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
                   PERFORM 23100-TRAIT-ENTER
              WHEN DFHPF3
                   MOVE PROGMENU TO PROG-SUIVANT
                   PERFORM 23050-PROG-SUIVANT 
              WHEN DFHPF12
                   PERFORM 23900-TRAIT-FIN
              WHEN OTHER
                   PERFORM 90000-ERR-TOUCHE
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
                  DELIMITED BY SIZE INTO MESS2O
           PERFORM 22000-TRAIT-ENVOI
           .
       23100-TRAIT-ENTER.
      *------------------*
           EXEC CICS RECEIVE MAP   (MA-MAP)
                             MAPSET(MON-MAPSET)
                             INTO  (MAP2I)
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
           IF NUM2I = LOW-VALUE 
              STRING 'INDIQUE LE NUMERO DE STAGIAIRE '
                  DELIMITED BY SIZE INTO MESS2O
              PERFORM  22000-TRAIT-ENVOI
           END-IF
           PERFORM 23400-LECTURE
           .

       23400-LECTURE.
      *---------------*
           EXEC CICS READ FILE  (MON-FICHIER)
                          RIDFLD(NUM2I)
                          INTO  (E-STAGIAIRE)
                          RESP  (C-R)
           END-EXEC
           EVALUATE C-R
             WHEN DFHRESP(NORMAL)
               MOVE E-NOM        TO NOM2I
               MOVE E-PRENOM     TO PNM2I
               MOVE E-DATE-NAISS TO DDN2I
               MOVE E-TELDOM     TO TF2I
               MOVE E-TELPOR     TO TM2I
               MOVE E-ADR1       TO ADS2I
               MOVE E-CODEP      TO CP2I  
               MOVE E-VILLE      TO VL2I  
               PERFORM 22000-TRAIT-ENVOI
             WHEN DFHRESP(NOTFND)
               MOVE 'STAGIAIRE NON TROUVE' TO MESS2O
               PERFORM  22000-TRAIT-ENVOI
             WHEN OTHER
               PERFORM 80000-APPEL-SPERR
               STRING 'ERREUR FICHIER ' TEXT-ERREUR DELIMITED BY SIZE
                      INTO MESS2O
               PERFORM  22000-TRAIT-ENVOI
           END-EVALUATE
            .
       23900-TRAIT-FIN.
      *---------------*
           MOVE 'FIN DE LA TRANSACTION' TO MESSAGE-TXT
           PERFORM   99000-FIN-CICS
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
                    INTO MESS2O
             PERFORM 22000-TRAIT-ENVOI
           END-IF
           EXEC CICS RECEIVE INTO(TEXT-ERREUR) RESP(C-R) END-EXEC
           IF C-R NOT = DFHRESP(NORMAL)
             MOVE C-R TO C-R-ED
             STRING 'ERREUR RECEIVE SPERR ' C-R-ED DELIMITED BY SIZE
                    INTO MESS2O
             PERFORM 22000-TRAIT-ENVOI
           END-IF
           .
       90000-ERR-TOUCHE.
      *----------------*
           MOVE 'TOUCHE DE FONCTION INVALIDE' TO MESS2I
           PERFORM  22000-TRAIT-ENVOI
           .
       91000-ERREUR-CICS.
      *-----------------*
           MOVE 'ERREUR CICS !!!  FIN DE LA TRANSACTION' TO MESSAGE-TXT
           PERFORM   99000-FIN-CICS
           .
       99000-FIN-CICS.
      *--------------*
           EXEC CICS SEND FROM   (MESSAGE-TXT)
                          LENGTH (LENGTH OF MESSAGE-TXT)
                          ERASE
           END-EXEC
           .
