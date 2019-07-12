      *Para uso com o FS
      *SET CALLFH"FHREDIR"

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HBSIS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       OBJECT SECTION.
       
       FILE-CONTROL.
       
      *    COPY CADCLI.SEL.
       
       DATA DIVISION.
       FILE SECTION.

      *    COPY CADCLI.FD.
      *>---------------------------------------------------------------<
       WORKING-STORAGE SECTION.

       78 DIALOG-SYSTEM                VALUE "DSGRUN".
       
       01 DISPLAY-ERROR-NO           PIC 9(004) VALUE ZEROS.
       01 DISPLAY-DETAILS-1          PIC 9(004) VALUE ZEROS.
       01 DISPLAY-DETAILS-2          PIC 9(004) VALUE ZEROS.
       77 IND                        PIC 999 VALUE 0 COMP-3.

       01 WK-DATA-SYS.
          02 WK-DATA-SISTEMA   PIC 9(08).
          02 FILLER REDEFINES WK-DATA-SISTEMA.
             03 WK-ANO-SISTEMA.
                04 WK-ANO-SYS  PIC 9(04).
             03 WK-MES-SYS     PIC 9(02).
             03 WK-DIA-SYS     PIC 9(02).
          02 WK-TIME.
             03 WK-HORA          PIC 9(08).
             03 FILLER REDEFINES WK-HORA.
               06 WK-DATE-HOR    PIC 9(02).
               06 WK-DATE-MIN    PIC 9(02).
               06 WK-DATE-SEG    PIC 9(02).
               06 WK-DATE-MIL    PIC 9(02).
             03 FILLER REDEFINES WK-HORA.
               06 WK-HORA-6      PIC 9(06).
               06 FILLER         PIC XX.

             03 wk-rest        PIC X(05).

       77 WS-LINHA-MSG				 PIC X(80).
       77 WS-PROCESSADOS                         PIC 9(05).
      *>---------------------------------------------------------------<
       COPY "DS-CNTRL.MF".
       COPY "HBSIS.CPB".
       COPY EDOBJ01.CPY.
       COPY HBSIS.CPY.
      *>---------------------------------------------------------------<
       PROCEDURE DIVISION.
       INICIO.
           PERFORM INICIALIZA-SCREENSET

           MOVE FUNCTION WHEN-COMPILED TO WK-DATA-SYS
           STRING "::HBSIS:: "
                  "V. " WK-DIA-SYS "/" WK-MES-SYS
                  "/" WK-ANO-SYS " " WK-DATE-HOR ":" WK-DATE-MIN
                  ":" WK-DATE-SEG INTO TITULO-D

           PERFORM CONTROLE UNTIL OPERACAO EQUAL "Sair"
           EXIT PROGRAM
           STOP RUN.
      *>---------------------------------------------------------------<
       INICIALIZA-SCREENSET.
           INITIALIZE DS-CONTROL-BLOCK DATA-BLOCK
           MOVE DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO
           MOVE VERSION-NO  TO DS-VERSION-NO
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "HBSIS"  TO DS-SET-NAME.
      *>---------------------------------------------------------------<
       CONTROLE.
           EVALUATE OPERACAO
               WHEN "Processa"
                   PERFORM PROCESSA

               WHEN "CADVEND"
                  CALL "CADVEND"
                  CANCEL "CADVEND"

               WHEN "CADCLI"
                  CALL "CADCLI"
                  CANCEL "CADCLI"


           END-EVALUATE
           MOVE SPACE TO OPERACAO
           PERFORM CALL-DIALOG-SYSTEM.
      *>---------------------------------------------------------------<
       CALL-DIALOG-SYSTEM.
       CALL DIALOG-SYSTEM USING DS-CONTROL-BLOCK DATA-BLOCK
          IF DS-ERROR-CODE NOT EQUAL ZEROS
               MOVE DS-ERROR-CODE      TO DISPLAY-ERROR-NO
               DISPLAY "DS ERROR NO:   "        DISPLAY-ERROR-NO
               DISPLAY "ERROR DETAILS(1) :   "  DISPLAY-DETAILS-1
               DISPLAY "ERROR DETAILS(2) :   "  DISPLAY-DETAILS-2
               STOP RUN.
      *>---------------------------------------------------------------<
       PROCESSA.
         INITIALIZE CAMPO-ERRO ERRO-LER-D WS-PROCESSADOS

         STRING " -- Manutenção no arquivo CADDUPR -- " INTO MENSAGEM-D.

      *  OPEN INPUT CADCLI
      *  IF FS NOT = "00"
      *    STRING "Erro - Abertura do arquivo CADCLI. St: " FS
      *    DELIMITED BY SIZE INTO CAMPO-ERRO
      *    MOVE "NAO" TO ERRO-LER-D
      *    GO TO SAI-PROCESSA
      *  END-IF
      *-----------------------------------------------------------------
