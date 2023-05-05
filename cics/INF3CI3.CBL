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
           05 TS-NOM.
             10 TS-NOM1           PIC X(4).
             10 TS-NOM2           PIC X(4).
           05 FILLER              PIC X(26).
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
           MOVE LOW-VALUE TO MAP1I
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
                  INTO TERM1O
           EXEC CICS ASKTIME ABSTIME(WS-TIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-TIME)
                               DDMMYYYY(DAT1O)
                               DATESEP ('/')
                               TIME (TIM1O)
                               TIMESEP(':')
           END-EXEC
           EXEC CICS SEND MAP    (MA-MAP)
                          MAPSET (MON-MAPSET)
                          FROM   (MAP1O)
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
                  DELIMITED BY SIZE INTO MESS11O
           PERFORM 22000-TRAIT-ENVOI
           .
       23100-TRAIT-ENTER.
      *------------------*
           EXEC CICS RECEIVE MAP   (MA-MAP)
                             MAPSET(MON-MAPSET)
                             INTO  (MAP1I)
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
           
           IF ETAPE NOT = '1'
             PERFORM 23300-CHECK-INPUT
             PERFORM 23400-ECRITURE
           ELSE 
             PERFORM 23450-ECRITURE-FIN 
           END-IF          
           .
       23200-TRAIT-FIN.
      *---------------*
           MOVE 'FIN DE LA TRANSACTION' TO MESSAGE-TXT
           PERFORM   99000-FIN-CICS
           .

       23300-CHECK-INPUT.
      *------------------*  
      *  CHECK NUMERO STAGIAIRE 
           IF NUM1I = LOW-VALUE 
              STRING 'NUMERO DE STAGIAIRE OBLIGATOIRE'
                  DELIMITED BY SIZE INTO MESS11O
              PERFORM  22000-TRAIT-ENVOI
           END-IF
           IF NUM1I < 1000 OR NUM1I > 5000
              STRING 'LE NUMERO DU STAGAIRE DOIT ETRE ENTRE 1000' 
                      ' ET 5000' DELIMITED BY SIZE INTO MESS11O
              PERFORM  22000-TRAIT-ENVOI
           END-IF 
      *  CHECK NOM STAGIAIRE
           IF NOM1I = LOW-VALUE 
              STRING 'NOM DU STAGIAIRE OBLIGATOIRE'
                  DELIMITED BY SIZE INTO MESS11O
              PERFORM  22000-TRAIT-ENVOI
           END-IF
      *  CHECK PRENOM STAGIAIRE
           IF PNM1I = LOW-VALUE 
              STRING 'PRENOM DU STAGIAIRE OBLIGATOIRE'
                  DELIMITED BY SIZE INTO MESS11O
              PERFORM  22000-TRAIT-ENVOI
           END-IF

      * CHECK SI VILLE OU ADRESSE INPUT SONT REMPLIS DE BLANC     
           IF ADS1I = SPACES THEN
             MOVE LOW-VALUE TO ADS1I
           END-IF            
           IF VL1I = SPACES THEN
             MOVE LOW-VALUE TO VL1I
           END-IF            
      *  CHECK ADRESSE COMPLETE OU VIDE 
           IF (ADS1I = LOW-VALUE  AND CP1I = LOW-VALUE AND
              VL1I = LOW-VALUE) OR (ADS1I NOT = LOW-VALUE AND 
              CP1I NOT = LOW-VALUE AND VL1I NOT = LOW-VALUE ) THEN
              CONTINUE   
           ELSE
              STRING 'ADRESSE INCOMPLETE'
                  DELIMITED BY SIZE INTO MESS11O
              PERFORM  22000-TRAIT-ENVOI
           END-IF

      *  CHECK TELEPHONE 
           IF TF1I NOT = LOW-VALUE 
             MOVE TF1I TO NUM-TEL
             IF NUM-TEL(1:2) NOT = 01 AND NOT = 02 AND NOT = 03 AND 
                   NOT = 04 AND NOT = 05 THEN
                STRING 'LE NUMERO DU TELEPHONE FIXE DOIT COMMENCE PAR'
                       ' 01 OU 02 OU 03 OU 04 OU 05' 
                      DELIMITED BY SIZE INTO MESS11O
                MOVE '  ' TO TF1I(1:2)   
                PERFORM  22000-TRAIT-ENVOI
             END-IF
           END-IF 
           IF TM1I NOT = LOW-VALUE 
             MOVE TM1I TO NUM-TEL
             IF NUM-TEL(1:2) NOT = 06 AND NOT = 07
                STRING 'LE NUMERO DU TELEPHONE MOBILE DOIT COMMENCE PAR'
                       ' 06 OU 07' 
                      DELIMITED BY SIZE INTO MESS11O
                MOVE '  ' TO TM1I(1:2)   
                PERFORM  22000-TRAIT-ENVOI
             END-IF
           END-IF 
      *  CHECK DATE DE NAISSANCE 
           IF DDN1I = LOW-VALUE 
              STRING 'DATE DE NAISSANCE DU STAGIAIRE OBLIGATOIRE'
                  DELIMITED BY SIZE INTO MESS11O
              PERFORM  22000-TRAIT-ENVOI
           END-IF
           MOVE DDN1I TO DDN-9
           IF DDN-9(1:2) < 01 
             STRING 'JOURS DE NAISSANCE INVALIDE'
                  DELIMITED BY SIZE INTO MESS11O
              PERFORM  22000-TRAIT-ENVOI
           END-IF
           IF (DDN-9(3:2) = '01' OR = '03' OR = '05' OR = '07' OR 
           = '08' OR = '10' OR = '12') AND DDN-9(1:2) > 31 THEN
             STRING 'JOURS DE NAISSANCE INVALIDE'
                  DELIMITED BY SIZE INTO MESS11O
              PERFORM  22000-TRAIT-ENVOI
           END-IF    
           IF DDN-9(3:2) = '02' AND DDN-9(1:2) > 29 THEN
             STRING 'JOURS DE NAISSANCE INVALIDE'
                  DELIMITED BY SIZE INTO MESS11O
              PERFORM  22000-TRAIT-ENVOI
           END-IF  
           IF (DDN-9(3:2) = '04' OR = '06' OR = '09' OR = '11') AND 
              DDN-9(1:2) > 30 THEN 
             STRING 'JOURS DE NAISSANCE INVALIDE'
                  DELIMITED BY SIZE INTO MESS11O
              PERFORM  22000-TRAIT-ENVOI
           END-IF 
           IF DDN-9(3:2) > 12
              STRING 'MOIS DE NAISSANCE INVALIDE'
                  DELIMITED BY SIZE INTO MESS11O
              PERFORM  22000-TRAIT-ENVOI
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
                  DELIMITED BY SIZE INTO MESS11O
              PERFORM  22000-TRAIT-ENVOI
           END-IF
           . 
           
       23400-ECRITURE.
      *------------------* 
           MOVE DFHBMASF   TO NUM1A
           MOVE DFHBMASF   TO NOM1A
           MOVE DFHBMASF   TO PNM1A 
           MOVE DFHBMASF   TO ADS1A
           MOVE DFHBMASF   TO CP1A
           MOVE DFHBMASF   TO VL1A
           MOVE DFHBMASF   TO TF1A
           MOVE DFHBMASF   TO TM1A
           MOVE DFHBMASF   TO DDN1A
           MOVE DFHBMASK   TO LCONF1A
           MOVE DFHBMFSE   TO CONF1A
           MOVE '1'        TO ETAPE
           PERFORM  22000-TRAIT-ENVOI
           .
       23450-ECRITURE-FIN.
      *------------------*      
           IF CONF1I NOT = 'Y' AND NOT = 'N'
             STRING 'CONFIRMATION INVALIDE' DELIMITED BY SIZE
               INTO MESS11O
             PERFORM  23400-ECRITURE
           END-IF
           IF CONF1I = 'Y'
             MOVE NUM1I TO E-NUMERO
             MOVE NOM1I TO E-NOM
             MOVE PNM1I TO E-PRENOM
             MOVE DDN1I TO E-DATE-NAISS
             IF TF1I NOT = LOW-VALUE 
               MOVE TF1I TO  E-TELDOM
             END-IF
             IF TM1I NOT = LOW-VALUE 
               MOVE TM1I TO  E-TELPOR
             END-IF
             IF CP1I NOT = LOW-VALUE
                MOVE ADS1I TO E-ADR1
                MOVE CP1I  TO E-CODEP  
                MOVE VL1I  TO E-VILLE  
             END-IF

             EXEC CICS WRITE FILE  (MON-FICHIER)
                             RIDFLD(E-NUMERO)
                             FROM  (E-STAGIAIRE)
                             RESP  (C-R)
             END-EXEC
             EVALUATE C-R 
               WHEN DFHRESP(NORMAL)
                 STRING 'STAGIARE ' NUM1I ' CREE' DELIMITED BY SIZE
                        INTO MESS11O
                 PERFORM 23900-RESET-INPUT
               WHEN DFHRESP(DUPREC)
                 MOVE 'STAGIARE DEJA EXISTANT' TO MESS11O 
               WHEN OTHER
                 PERFORM 80000-APPEL-SPERR
                 STRING 'ERREUR FICHIER WRITE ' C-R-ED DELIMITED BY SIZE
                         INTO MESS11O
             END-EVALUATE
           ELSE
             MOVE 'CREATION ABANDONEE' TO MESS11O
           END-IF
           MOVE ' ' TO ETAPE
           MOVE 'N' TO CONF1O 
           PERFORM 22000-TRAIT-ENVOI
           .

       23900-RESET-INPUT.
      *------------------* 
           MOVE LOW-VALUE TO NUM1I
                             NOM1I
                             PNM1I
                             ADS1I
                             CP1I
                             VL1I
                             TF1I
                             TM1I
                             DDN1I
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
                    INTO MESS11O
             PERFORM 22000-TRAIT-ENVOI
           END-IF
           EXEC CICS RECEIVE INTO(TEXT-ERREUR) RESP(C-R) END-EXEC
           IF C-R NOT = DFHRESP(NORMAL)
             MOVE C-R TO C-R-ED
             STRING 'ERREUR RECEIVE SPERR ' C-R-ED DELIMITED BY SIZE
                    INTO MESS11O
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
