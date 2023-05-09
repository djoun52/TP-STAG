       IDENTIFICATION DIVISION.
       PROGRAM-ID. INF1CI3.
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
           05 MESSPROG            PIC X(17) VALUE LOW-VALUE.
           05 TS-NOM.
             10 TS-NOM1           PIC X(4).
             10 TS-NOM2           PIC X(4).

       01 C-R                     PIC S9(8) COMP.
       01 C-R-ED                  PIC ZZZ9.
       01 WS-TIME                 PIC X(8).
       01 TEXT-ERREUR             PIC X(15).
       01 MON-PROG                PIC X(8) VALUE 'INF3CI3 '.
       01 PROGMENU                PIC X(8) VALUE 'INF0CI3 '.
       01 SPERR                   PIC X(8) VALUE 'SPERR3  '.
       01 MON-FICHIER             PIC X(8) VALUE 'FSTAG03 '.
       01 MA-MAP                  PIC X(8) VALUE 'MAP3    '.
       01 MON-MAPSET              PIC X(8) VALUE 'MAPC3   '.
       01 MA-TRX                  PIC X(4) VALUE 'T3C3'.
       01 NUM-TEL                 PIC X(10).
       01 DDN-9                   PIC 9(8).
       01 ADN-9                   PIC 9(4).
       01 DAT-9                   PIC 9(4).
       01 DAT                     PIC 9(8).
       01 AGE-OK                  PIC 9999.
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
            GOBACK
            .
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
           MOVE SPACES TO E-STAGIAIRE
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
           MOVE LOW-VALUE TO MAP3I
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
                  INTO TERM3O
           EXEC CICS ASKTIME ABSTIME(WS-TIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-TIME)
                               DDMMYYYY(DAT3O)
                               DATESEP ('/')
                               TIME (TIM3O)
                               TIMESEP(':')
           END-EXEC
           EXEC CICS SEND MAP    (MA-MAP)
                          MAPSET (MON-MAPSET)
                          FROM   (MAP3O)
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
                   PERFORM 23200-TRAIT-FIN
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
                  DELIMITED BY SIZE INTO MESS31O
           PERFORM 22000-TRAIT-ENVOI
           .
       23100-TRAIT-ENTER.
      *------------------*
           EXEC CICS RECEIVE MAP   (MA-MAP)
                             MAPSET(MON-MAPSET)
                             INTO  (MAP3I)
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
           EVALUATE ETAPE
             WHEN '1'
               PERFORM 23300-CHECK-INPUT
               PERFORM 23430-MODIFICATION 
             WHEN '2'
               PERFORM 23450-MODIFICATION-FIN 
             WHEN OTHER
               PERFORM 23400-MODIFICATION-DEBUT
           END-EVALUATE
                      
           .
       23200-TRAIT-FIN.
      *---------------*
           MOVE 'FIN DE LA TRANSACTION' TO MESSAGE-TXT
           PERFORM   99000-FIN-CICS
           .

       23300-CHECK-INPUT.
      *------------------*  
      *  CHECK NUMERO STAGIAIRE 
           IF NUM3I = LOW-VALUE 
              STRING 'NUMERO DE STAGIAIRE OBLIGATOIRE'
                  DELIMITED BY SIZE INTO MESS31O
              PERFORM  23400-MODIFICATION-DEBUT
           END-IF
           IF NUM3I < 1000 OR NUM3I > 5000
              STRING 'LE NUMERO DU STAGAIRE DOIT ETRE ENTRE 1000' 
                      ' ET 5000' DELIMITED BY SIZE INTO MESS31O
              PERFORM  23400-MODIFICATION-DEBUT
           END-IF 
      *  CHECK NOM STAGIAIRE
           IF NOM3I = LOW-VALUE 
              STRING 'NOM DU STAGIAIRE OBLIGATOIRE'
                  DELIMITED BY SIZE INTO MESS31O
              PERFORM  23400-MODIFICATION-DEBUT
           END-IF
      *  CHECK PRENOM STAGIAIRE
           IF PNM3I = LOW-VALUE 
              STRING 'PRENOM DU STAGIAIRE OBLIGATOIRE'
                  DELIMITED BY SIZE INTO MESS31O
              PERFORM  23400-MODIFICATION-DEBUT
           END-IF

      * CHECK SI VILLE OU ADRESSE INPUT SONT REMPLIS DE BLANC     
           IF ADS3I = SPACES THEN
             MOVE LOW-VALUE TO ADS3I
           END-IF            
           IF VL3I = SPACES THEN
             MOVE LOW-VALUE TO VL3I
           END-IF            
      *  CHECK ADRESSE COMPLETE OU VIDE 
           IF (ADS3I = LOW-VALUE  AND CP3I = LOW-VALUE AND
              VL3I = LOW-VALUE) OR (ADS3I NOT = LOW-VALUE AND 
              CP3I NOT = LOW-VALUE AND VL3I NOT = LOW-VALUE ) THEN
              CONTINUE   
           ELSE
              STRING 'ADRESSE INCOMPLETE'
                  DELIMITED BY SIZE INTO MESS31O
              PERFORM  23400-MODIFICATION-DEBUT
           END-IF

      *  CHECK TELEPHONE 
           IF TF3I NOT = LOW-VALUE 
             MOVE TF3I TO NUM-TEL
             IF NUM-TEL(1:2) NOT = 01 AND NOT = 02 AND NOT = 03 AND 
                   NOT = 04 AND NOT = 05 THEN
                STRING 'LE NUMERO DU TELEPHONE FIXE DOIT COMMENCE PAR'
                       ' 01 OU 02 OU 03 OU 04 OU 05' 
                      DELIMITED BY SIZE INTO MESS31O
                MOVE '  ' TO TF3I(1:2)   
                PERFORM  23400-MODIFICATION-DEBUT
             END-IF
           END-IF 
           IF TM3I NOT = LOW-VALUE 
             MOVE TM3I TO NUM-TEL
             IF NUM-TEL(1:2) NOT = 06 AND NOT = 07
                STRING 'LE NUMERO DU TELEPHONE MOBILE DOIT COMMENCE PAR'
                       ' 06 OU 07' 
                      DELIMITED BY SIZE INTO MESS31O
                MOVE '  ' TO TM3I(1:2)   
                PERFORM  23400-MODIFICATION-DEBUT
             END-IF
           END-IF 
      *  CHECK DATE DE NAISSANCE 
           IF DDN3I = LOW-VALUE 
              STRING 'DATE DE NAISSANCE DU STAGIAIRE OBLIGATOIRE'
                  DELIMITED BY SIZE INTO MESS31O
              PERFORM  23400-MODIFICATION-DEBUT
           END-IF
           MOVE DDN3I TO DDN-9
           IF DDN-9(1:2) < 01 
             STRING 'JOURS DE NAISSANCE INVALIDE'
                  DELIMITED BY SIZE INTO MESS31O
              PERFORM  23400-MODIFICATION-DEBUT
           END-IF
           IF (DDN-9(3:2) = '01' OR = '03' OR = '05' OR = '07' OR 
           = '08' OR = '10' OR = '12') AND DDN-9(1:2) > 31 THEN
             STRING 'JOURS DE NAISSANCE INVALIDE'
                  DELIMITED BY SIZE INTO MESS31O
              PERFORM  23400-MODIFICATION-DEBUT
           END-IF    
           IF DDN-9(3:2) = '02' AND DDN-9(1:2) > 29 THEN
             STRING 'JOURS DE NAISSANCE INVALIDE'
                  DELIMITED BY SIZE INTO MESS31O
              PERFORM  23400-MODIFICATION-DEBUT
           END-IF  
           IF (DDN-9(3:2) = '04' OR = '06' OR = '09' OR = '11') AND 
              DDN-9(1:2) > 30 THEN 
             STRING 'JOURS DE NAISSANCE INVALIDE'
                  DELIMITED BY SIZE INTO MESS31O
              PERFORM  23400-MODIFICATION-DEBUT
           END-IF 
           IF DDN-9(3:2) > 12
              STRING 'MOIS DE NAISSANCE INVALIDE'
                  DELIMITED BY SIZE INTO MESS31O
              PERFORM  23400-MODIFICATION-DEBUT
           END-IF
           EXEC CICS ASKTIME ABSTIME(WS-TIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-TIME)
                               YYYYMMDD(DAT)
           END-EXEC
           MOVE DDN-9(5:4) TO ADN-9
           MOVE DAT(1:4)   TO DAT-9
           SUBTRACT ADN-9 FROM DAT-9 GIVING AGE-OK
           IF AGE-OK < 17 OR AGE-OK > 30              
              STRING 'ANNEE DE NAISSANCE INVALIDE ' DAT-9 ' '  AGE-OK
                  DELIMITED BY SIZE INTO MESS31O
              PERFORM  23400-MODIFICATION-DEBUT
           END-IF
           . 
       23400-MODIFICATION-DEBUT.
      *------------------------*
           EXEC CICS READ FILE  (MON-FICHIER)
                          RIDFLD(NUM3I)
                          INTO  (E-STAGIAIRE)
                          RESP  (C-R)
           END-EXEC
           EVALUATE C-R
             WHEN DFHRESP(NORMAL)
               MOVE E-NOM        TO NOM3I
               MOVE E-PRENOM     TO PNM3I
               MOVE E-DATE-NAISS TO DDN3I
               IF E-TELDOM NOT = SPACES
                 MOVE E-TELDOM     TO TF3I
               END-IF
               IF E-TELPOR NOT = SPACES
                 MOVE E-TELPOR     TO TM3I
               END-IF
               IF E-ADR1 NOT = SPACES
                 MOVE E-ADR1       TO ADS3I
                 MOVE E-CODEP      TO CP3I  
                 MOVE E-VILLE      TO VL3I
               END-IF
               MOVE DFHBMASF     TO NUM3A
               MOVE DFHBMFSE     TO NOM3A
               MOVE DFHBMFSE     TO PNM3A 
               MOVE DFHBMFSE     TO ADS3A
               MOVE DFHBMFSE     TO CP3A
               MOVE DFHBMFSE     TO VL3A
               MOVE DFHBMFSE     TO TF3A
               MOVE DFHBMFSE     TO TM3A
               MOVE DFHBMFSE     TO DDN3A  
               MOVE 'VOUS POUVEZ MODIFIEZ LES DONNEES' TO OPER3O
               MOVE '1'        TO ETAPE
               PERFORM  22000-TRAIT-ENVOI
             WHEN DFHRESP(NOTFND)
               MOVE 'STAGIAIRE NON TROUVE' TO MESS2O
             WHEN OTHER
               PERFORM 80000-APPEL-SPERR
               STRING 'ERREUR FICHIER ' TEXT-ERREUR DELIMITED BY SIZE
                      INTO MESS2O
           END-EVALUATE
           .
       23430-MODIFICATION.
      *------------------* 
           MOVE DFHBMASF   TO NUM3A
           MOVE DFHBMASF   TO NOM3A
           MOVE DFHBMASF   TO PNM3A 
           MOVE DFHBMASF   TO ADS3A
           MOVE DFHBMASF   TO CP3A
           MOVE DFHBMASF   TO VL3A
           MOVE DFHBMASF   TO TF3A
           MOVE DFHBMASF   TO TM3A
           MOVE DFHBMASF   TO DDN3A
           MOVE DFHBMASK   TO LCONF3A
           MOVE DFHBMFSE   TO CONF3A
           MOVE '2'        TO ETAPE
           PERFORM  22000-TRAIT-ENVOI
           .
       23450-MODIFICATION-FIN.
      *------------------*      
           IF CONF3I NOT = 'Y' AND NOT = 'N'
             STRING 'CONFIRMATION INVALIDE' DELIMITED BY SIZE
               INTO MESS31O
             PERFORM  23430-MODIFICATION
           END-IF
           IF CONF3I = 'Y'
             MOVE NUM3I TO E-NUMERO
             EXEC CICS READ  FILE  (MON-FICHIER)
                             RIDFLD(E-NUMERO)
                             INTO  (E-STAGIAIRE)
                             UPDATE
                             RESP  (C-R)
             END-EXEC
             IF C-R = DFHRESP(NORMAL)
               MOVE NUM3I TO E-NUMERO
               MOVE NOM3I TO E-NOM
               MOVE PNM3I TO E-PRENOM
               MOVE DDN3I TO E-DATE-NAISS
               IF TF3I NOT = LOW-VALUE 
                 MOVE TF3I TO  E-TELDOM
               ELSE 
                 MOVE SPACES TO E-TELDOM
               END-IF
               IF TM3I NOT = LOW-VALUE 
                 MOVE TM3I TO  E-TELPOR
               ELSE 
                 MOVE SPACES TO E-TELPOR
               END-IF
               IF CP3I NOT = LOW-VALUE
                  MOVE ADS3I TO E-ADR1
                  MOVE CP3I  TO E-CODEP  
                  MOVE VL3I  TO E-VILLE 
               ELSE 
                  MOVE SPACES TO E-ADR1
                                 E-CODEP  
                                 E-VILLE  
               END-IF
               EXEC CICS REWRITE FILE  (MON-FICHIER)
                                 FROM  (E-STAGIAIRE)
                                 RESP  (C-R)
               END-EXEC
               IF C-R = DFHRESP(NORMAL)
                 STRING 'STAGIAIRE ' NUM3I ' MODIFIE' DELIMITED BY SIZE
                         INTO MESSPROG
                 MOVE PROGMENU TO PROG-SUIVANT
                 PERFORM 23050-PROG-SUIVANT 
               ELSE
                 PERFORM 80000-APPEL-SPERR
                 STRING 'ERREUR FICHIER REWRITE' C-R-ED
                         DELIMITED BY SIZE INTO MESS31O
               END-IF
             ELSE
                 PERFORM 80000-APPEL-SPERR
                 STRING 'ERREUR FICHIER READ UPDATE'  C-R-ED
                         DELIMITED BY SIZE INTO MESS31O
             END-IF
           ELSE
             MOVE 'MODIFICATION ABANDONEE' TO MESS31O
           END-IF
           MOVE ' ' TO ETAPE
           MOVE 'N' TO CONF3O 
           PERFORM 22000-TRAIT-ENVOI
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
                    INTO MESS31O
             PERFORM 22000-TRAIT-ENVOI
           END-IF
           EXEC CICS RECEIVE INTO(TEXT-ERREUR) RESP(C-R) END-EXEC
           IF C-R NOT = DFHRESP(NORMAL)
             MOVE C-R TO C-R-ED
             STRING 'ERREUR RECEIVE SPERR ' C-R-ED DELIMITED BY SIZE
                    INTO MESS31O
             PERFORM 22000-TRAIT-ENVOI
           END-IF
           .
       90000-ERR-TOUCHE.
      *----------------*
           MOVE 'TOUCHE DE FONCTION INVALIDE' TO MESS11I
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
