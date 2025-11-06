       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAIR-IMPAIR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NOMBRE-CHAINE  PIC X(10).
       01 NOMBRE         PIC 9(5) VALUE 0.
       01 RESTE          PIC 9 VALUE 0.
       01 I              PIC 9 VALUE 1.
       01 CAR            PIC X.
       01 VALIDE         PIC X VALUE "O".   *> "O" = Oui, "N" = Non
       01 LONGUEUR       PIC 9 VALUE 0.

       PROCEDURE DIVISION.
           DISPLAY "ENTREZ UN NOMBRE : ".
           ACCEPT NOMBRE-CHAINE.

           MOVE FUNCTION LENGTH(FUNCTION TRIM(NOMBRE-CHAINE))
                TO LONGUEUR.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LONGUEUR
               MOVE NOMBRE-CHAINE(I:1) TO CAR
               IF CAR < "0" OR CAR > "9"
                   MOVE "N" TO VALIDE
               END-IF
           END-PERFORM

           IF VALIDE = "N"
               DISPLAY "ERREUR : VOUS DEVEZ ENTRER UN NOMBRE VALIDE."
           ELSE
               MOVE FUNCTION NUMVAL(NOMBRE-CHAINE) TO NOMBRE
               COMPUTE RESTE = FUNCTION MOD(NOMBRE, 2)
               IF RESTE = 0
                   DISPLAY "LE NOMBRE " NOMBRE " EST PAIR."
               ELSE
                   DISPLAY "LE NOMBRE " NOMBRE " EST IMPAIR."
               END-IF
           END-IF.

           STOP RUN.
