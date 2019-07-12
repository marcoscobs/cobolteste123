       
      *>------------ -- ---------------------------------------------
      *> Class description
      *>-----------------------------------------------------------
       class-id. edcnpj data is private
                 inherits from base.

       object section.
       class-control.
           edcnpj   is class "edcnpj"
      *> OCWIZARD - start list of classes
           base is class "base"
      *> OCWIZARD - end list of classes
      *>---USER-CODE. Add any additional class names below.
           .

       file-control.
         *> *.sel

       data division.
         *> *.fd
      *>-----------------------------------------------------------
       working-storage section. *> Dados globais.

       77 MSG-RET               PIC X(76) VALUE SPACE.

       77 WS-QUOCIENTE       PIC 99 VALUE 0.
       77 WS-RESTO           PIC 99 VALUE 0.
       77 WS-SOMA            PIC 999  VALUE 0.
       77 WS-SOMA-GERAL      PIC 9999 VALUE 0.
       77 INDX               PIC 999  VALUE 0.
       77 indice             pic 99   value 0.

       01 WS-CHEK.
           02 DIG-1-CALC         PIC 9 VALUE 0.
           02 DIG-2-CALC         PIC 9 VALUE 0.

      *Fim

      *>-----------------------------------------------------------

      *>-----------------------------------------------------------
       class-object.   *> Métodos e objetos internos.
      *>-----------------------------------------------------------
       object-storage section.

      *> OCWIZARD - start standard class methods
      *> OCWIZARD - end standard class methods

       end class-object.

      *>-----------------------------------------------------------
       object.         *> Métodos e objetos vistos externamente
      *>-----------------------------------------------------------
       object-storage section.

       METHOD-ID. "GET-MSG-RET".
       LINKAGE SECTION.
       77  MSG-RET-R     PIC X(76).

       PROCEDURE DIVISION RETURNING MSG-RET-R.

         MOVE MSG-RET TO MSG-RET-R.

         EXIT METHOD.

       END METHOD "GET-MSG-RET".

       METHOD-ID. "CPF".
       LINKAGE SECTION.

       01 WS-CPF             PIC 9(11).
       01 FILLER REDEFINES WS-CPF.
          02 WS-CPF-9   PIC 9(09).
          02 FILLER REDEFINES WS-CPF-9.
             04 WS-DIG-1 PIC 9 OCCURS 9 TIMES.
          02 WS-C1      PIC 9.
          02 WS-C2      PIC 9.

       PROCEDURE DIVISION USING WS-CPF.

           MOVE SPACE TO MSG-RET

           MOVE 0 TO WS-SOMA WS-SOMA-GERAL INDX.
           MOVE 10 TO INDICE.
        LOOP-INDX.
           ADD 1 TO INDX.
           MULTIPLY WS-DIG-1 (INDX) BY INDICE GIVING WS-SOMA
           ADD WS-SOMA  TO WS-SOMA-GERAL.
           SUBTRACT 1 FROM INDICE
           IF INDICE NOT = 1
               GO TO LOOP-INDX.
           DIVIDE WS-SOMA-GERAL BY 11 GIVING WS-QUOCIENTE.
           COMPUTE WS-RESTO = WS-SOMA-GERAL - (WS-QUOCIENTE * 11)

           IF WS-RESTO = 1 OR 0
              MOVE 0 TO DIG-1-CALC
           ELSE
              COMPUTE DIG-1-CALC = 11 - WS-RESTO.

           MOVE 0 TO INDX WS-SOMA WS-SOMA-GERAL
           MOVE 11 TO INDICE.
        LOOP-INDX-2.
           ADD 1 TO INDX.
           MULTIPLY WS-DIG-1 (INDX) BY INDICE GIVING WS-SOMA
           ADD WS-SOMA  TO WS-SOMA-GERAL.
           SUBTRACT 1 FROM INDICE
           IF INDICE NOT = 2
               GO TO LOOP-INDX-2.

           MULTIPLY DIG-1-CALC BY INDICE GIVING WS-SOMA
           ADD WS-SOMA  TO WS-SOMA-GERAL.

           DIVIDE WS-SOMA-GERAL BY 11 GIVING WS-QUOCIENTE.
           COMPUTE WS-RESTO = WS-SOMA-GERAL - (WS-QUOCIENTE * 11)

           IF WS-RESTO = 1 OR 0
              MOVE 0 TO DIG-2-CALC
           ELSE
              COMPUTE DIG-2-CALC = 11 - WS-RESTO.

            IF WS-C1 NOT = DIG-1-CALC
            OR WS-C2 NOT = DIG-2-CALC
                MOVE "Atenção! Número do CPF. Incorreto  "
                                                       TO MSG-RET.
           EXIT METHOD.
       END METHOD "CPF".

       METHOD-ID. "CNPJ".
       LINKAGE SECTION.

       01 WS-CGC             PIC 9(14).
       01 FILLER REDEFINES WS-CGC.
          02 WS-CGC-12   PIC 9(12).
          02 FILLER REDEFINES WS-CGC-12.
             04 WS-DIG-1-CGC  PIC 9 OCCURS 12 TIMES.
          02 WS-C1-CGC   PIC 9.
          02 WS-C2-CGC   PIC 9.

       PROCEDURE DIVISION USING WS-CGC.

           MOVE SPACE TO MSG-RET
           MOVE 0 TO WS-SOMA WS-SOMA-GERAL INDX.
           MOVE 05 TO INDICE.
        LOOP-INDX-5432.
           ADD 1 TO INDX.
           MULTIPLY WS-DIG-1-CGC (INDX) BY INDICE GIVING WS-SOMA
           ADD WS-SOMA  TO WS-SOMA-GERAL.

           SUBTRACT 1 FROM INDICE
           IF INDICE NOT = 1
               GO TO LOOP-INDX-5432.

           MOVE 09 TO INDICE.
           MOVE 04 TO INDX.
        LOOP-INDX-9876.
           ADD 1 TO INDX.
           MULTIPLY WS-DIG-1-CGC (INDX) BY INDICE GIVING WS-SOMA
           ADD WS-SOMA  TO WS-SOMA-GERAL.

           SUBTRACT 1 FROM INDICE
           IF INDICE NOT = 5
               GO TO LOOP-INDX-9876.

           MOVE 05 TO INDICE.
           MOVE 08 TO INDX.
        LOOP-INDX-5432-1.
           ADD 1 TO INDX.
           MULTIPLY WS-DIG-1-CGC (INDX) BY INDICE GIVING WS-SOMA
           ADD WS-SOMA  TO WS-SOMA-GERAL.

           SUBTRACT 1 FROM INDICE
           IF INDICE NOT = 1
               GO TO LOOP-INDX-5432-1.

           DIVIDE WS-SOMA-GERAL BY 11 GIVING WS-QUOCIENTE.
           COMPUTE WS-RESTO = WS-SOMA-GERAL - (WS-QUOCIENTE * 11)

           IF WS-RESTO NOT = 0
              COMPUTE DIG-1-CALC = 11 - WS-RESTO
           ELSE
              MOVE 0  TO DIG-1-CALC.

      ****  2o. Digito...........................

           MOVE 0 TO INDX WS-SOMA WS-SOMA-GERAL
           MOVE 06 TO INDICE.
        LOOP-INDX-65432.
           ADD 1 TO INDX.
           MULTIPLY WS-DIG-1-CGC (INDX) BY INDICE GIVING WS-SOMA
           ADD WS-SOMA  TO WS-SOMA-GERAL.
           SUBTRACT 1 FROM INDICE
           IF INDICE NOT = 1
               GO TO LOOP-INDX-65432.

           MOVE 09 TO INDICE.
        LOOP-INDX-987.
           ADD 1 TO INDX.
           MULTIPLY WS-DIG-1-CGC (INDX) BY INDICE GIVING WS-SOMA
           ADD WS-SOMA  TO WS-SOMA-GERAL.
           SUBTRACT 1 FROM INDICE
           IF INDICE NOT = 6
               GO TO LOOP-INDX-987.

           MOVE 06 TO INDICE.

        LOOP-INDX-6543.
           ADD 1 TO INDX.
           MULTIPLY WS-DIG-1-CGC (INDX) BY INDICE GIVING WS-SOMA
           ADD WS-SOMA  TO WS-SOMA-GERAL.
           SUBTRACT 1 FROM INDICE
           IF INDICE NOT = 2
               GO TO LOOP-INDX-6543.

           MULTIPLY DIG-1-CALC BY INDICE GIVING WS-SOMA.
           ADD WS-SOMA TO  WS-SOMA-GERAL.

           DIVIDE WS-SOMA-GERAL BY 11 GIVING WS-QUOCIENTE.
           COMPUTE WS-RESTO = WS-SOMA-GERAL - (WS-QUOCIENTE * 11)

           IF WS-RESTO NOT = 0
              COMPUTE DIG-2-CALC = 11 - WS-RESTO
           ELSE
              MOVE 0  TO DIG-2-CALC.

           MOVE 0 TO INDX WS-SOMA WS-SOMA-GERAL

            IF WS-C1-CGC NOT = DIG-1-CALC
            OR WS-C2-CGC NOT = DIG-2-CALC
               MOVE "Atenção! Número do CNPJ. Incorreto  " TO MSG-RET.

           EXIT METHOD.
       END METHOD "CNPJ".

       end object.
       end class edcnpj.
