      *Para uso com o FS
      *SET CALLFH"FHREDIR"

       COPY DSLANG.CPY.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CADCLI.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CALL-CONVENTION 66 IS CC66.
       CLASS-CONTROL.
           EDOBJ01 IS CLASS "edobj01"
           EDCNPJ class "edcnpj".

       FILE-CONTROL.
          COPY CLIENTE.SEL.

          SELECT CSV-FILE ASSIGN TO WS-NOME-CSV
          ORGANIZATION IS LINE SEQUENTIAL
          FILE STATUS IS FS.

       DATA DIVISION.
       FILE SECTION.
          COPY CLIENTE.FD.

          FD CSV-FILE.
          01 REG-CSV.
            02 LINHA-CSV PIC X(4791).

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.

      * Objecto.
       01 OBJ-IMP                  OBJECT REFERENCE.
       01 OBJ-DB                   OBJECT REFERENCE.
       77 WS-MSG-RET               PIC X(76).
       77 WS-RETORNO               PIC X.
       77 WS-COD                   PIC 999  VALUE 0.
      *-----------------------------------------------------------------
       77 WS-ID-WEB-SERVICE        PIC X.
       77 WS-COD-META              PIC 9(06).

       01 WS-FUNC.
          02 FILLER                PIC X(07) VALUE "AP-FUNC".

       01 DT-INV                   PIC 9(08).
       01 FILLER REDEFINES DT-INV.
          02 ANO-INV               PIC 9(04).
          02 MES-INV               PIC 99.
          02 DIA-INV               PIC 99.

       77 WS-DT-FIM-ANTERIOR       PIC 9(08).
       77 WS-DT-INI-ANTERIOR       PIC 9(08).

       77 WS-DT-I                  PIC 99/99/9999.
       77 WS-DT-F                  PIC 99/99/9999.
       77 VR-EDIT                  PIC ZZZ,ZZZ,ZZ9.99.
       77 WS-LAT-EDIT                PIC -ZZ9.99999999.
       77 WS-LONG-EDIT               PIC -ZZ9.99999999.
       77 WS-LAT-TEXT                PIC X(15).
       77 WS-LONG-TEXT               PIC X(15).
       77 WS-NOME-CSV                PIC X(500).
       77 WS-COD-CLI                 PIC 9(07).
      *77 LER-VENDEDOR-DB          PIC 9.
      *-----------------------------------------------------------------
       78 DIALOG-SYSTEM            VALUE "DSGRUN".
       77 BYTE                     PIC 9(003) VALUE ZEROS.
       77 ENCONTRADO               PIC 9(002) VALUE ZEROS.
       77 PESQBACK                 PIC X(0139) VALUE SPACES.
       77 LENSTRING                PIC X(004) COMP-5 VALUE ZEROS.

       01 DISPLAY-ERROR-NO         PIC 9(004) VALUE ZEROS.
       01 DISPLAY-DETAILS-1        PIC 9(004) VALUE ZEROS.
       01 DISPLAY-DETAILS-2        PIC 9(004) VALUE ZEROS.
      *-----------------------------------------------------------------
       01 FILLER PIC 9 VALUE 0.
          88 WS-USA-MERCURIO VALUE 1 FALSE 0.

       77 WS-LISTA                 OBJECT REFERENCE.

       01 OBJ-CNPJ                 OBJECT REFERENCE.

       01 WS-SPACES                   PIC 9(05).
       01 WS-STRING-LEN               PIC 9(05).
       01 BUFFER PIC X(260).
       01 USER-TITLE PIC X(25).
       01 LPITEMIDLIST POINTER.
       01 PSZPATH PIC X(260).
       01 RET-CODE PIC X(4) COMP-5.
       01 BROWSE-INFO.
          05 HWNDOWNER PIC X(4) COMP-5.
          05 PIDLROOT POINTER VALUE NULL.
          05 PSZDISPLAYNAME POINTER.
          05 LPSZTITLE POINTER.
          05 ULFLAGS PIC X(4) COMP-5 VALUE 0.
          05 LPFN POINTER VALUE NULL.
          05 LPARAM PIC X(4) COMP-5 VALUE 0.
          05 IIMAGE PIC X(4) COMP-5 VALUE 0.

      *-----------------------------------------------------------------
       COPY "DS-CNTRL.MF".
       COPY "CADCLI.CPB".
       COPY CADCLI.CPY.
       COPY ED-LIST.CPB.
       COPY WSINV.R32.
       COPY EDOBJ01.CPY.
      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       procedure division.

       INICIO.
          PERFORM INICIALIZA-SCREENSET

          OPEN INPUT CLIENTE
          IF FS = "35" OR FS = "05"
             OPEN OUTPUT CLIENTE
          END-IF
          CLOSE CLIENTE

          PERFORM CONTROLE UNTIL OPERACAO EQUAL "Sair"
          EXIT PROGRAM.
          STOP RUN.
      *-----------------------------------------------------------------
       INICIALIZA-SCREENSET.
          INITIALIZE DS-CONTROL-BLOCK DATA-BLOCK
          MOVE DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO
          MOVE VERSION-NO  TO DS-VERSION-NO
          MOVE DS-PUSH-SET TO DS-CONTROL
          MOVE "CADCLI"     TO DS-SET-NAME.
      *-----------------------------------------------------------------
       CONTROLE.
          EVALUATE FUNCTION UPPER-CASE(OPERACAO)
             WHEN "INICIALIZA-OBJETOS"
                PERFORM INICIALIZA-OBJETOS
             WHEN "SET-FOCUS"
                MOVE "SET-FOCUS"        TO CALL-FUNCTION
                SET OBJECT-REFERENCE    TO WS-LISTA
                CALL "ED-LIST" USING DATA-BLOCK-LV
             WHEN "LISTAR"
                PERFORM LISTAR
             WHEN "GRAVAR"
                PERFORM GRAVAR
             WHEN "SELECIONAR"
                PERFORM SELECIONAR
             WHEN "EXCLUIR"
                PERFORM EXCLUIR
             WHEN "PESQUISAR"
                PERFORM PESQUISAR
             WHEN "ADICIONAR"
                PERFORM ADICIONAR
             WHEN "EDITAR"
                PERFORM EDITAR
             WHEN "CONFIGURA-SELECAO"
                PERFORM CONFIGURA-SELECAO
             WHEN "IMPORTAR"
                PERFORM IMPORTAR
             WHEN "REMOVER-TODOS"
                PERFORM REMOVER-TODOS
          END-EVALUATE

          MOVE SPACE TO OPERACAO

          PERFORM CALL-DIALOG-SYSTEM.
      *-----------------------------------------------------------------
       INICIALIZA-OBJETOS.

          MOVE   "CADCLI"        TO OO-PROGRAMA-LISTA
          invoke EdObj01 "Configuracao" using Objetos
          MOVE-OBJECT-HANDLE WIN-CLIENTE OO-Handle-Objeto

          MOVE 0                  TO y

          ADD 1                   TO y
          MOVE "Código"           TO Lvitem-text  (y)
          MOVE 5                  TO lvitem-length(y)

          ADD 1                   TO y
          MOVE "CNPJ"             TO Lvitem-text  (y)
          MOVE 10                 TO lvitem-length(y)

          ADD 1                   TO y
          MOVE "Razão Social"     TO Lvitem-text  (y)
          MOVE 30                 TO lvitem-length(y)

          ADD 1                   TO y
          MOVE "Latitude"         TO Lvitem-text  (y)
          MOVE 10                 TO lvitem-length(y)

          ADD 1                   TO y
          MOVE "Longitude"        TO Lvitem-text  (y)
          MOVE 10                 TO lvitem-length(y)

          SET WS-LISTA            TO lv-lista
          SET object-reference    TO WS-LISTA
          move y                  to numeric-value
          MOVE "add-header"       TO call-function
          CALL "ED-LIST" using data-block-lv.
      *-----------------------------------------------------------------
       PESQUISAR.
          INVOKE EDOBJ01 "Pesquisar" USING DATA-BLOCK-LV WS-LISTA PESQ
          CONTINUE.
      *-----------------------------------------------------------------
       EXCLUIR.

          INITIALIZE CAMPO-ERRO ERRO-LER-D

          OPEN I-O CLIENTE

          PERFORM PREENCHE-CHAVE

          SET OBJECT-REFERENCE     TO WS-LISTA
          MOVE "DELETE-LIST-ITEM"  TO CALL-FUNCTION
          CALL "ED-LIST"  USING DATA-BLOCK-LV

          MOVE COD-CLI-D  TO COD-CLI
          READ CLIENTE WITH LOCK

          IF FS = "23"
             MOVE "NAO" TO ERRO-LER-D
             STRING "Atenção! Nenhum registro selecionado. FS: " FS
             DELIMITED BY SIZE INTO CAMPO-ERRO
             CLOSE CLIENTE
             EXIT PARAGRAPH
          END-IF

          IF FS NOT = ZEROS
             MOVE "NAO" TO ERRO-LER-D
             STRING "Atenção! Erro ao ler registro. FS: " FS
             DELIMITED BY SIZE INTO CAMPO-ERRO
             CLOSE CLIENTE
             EXIT PARAGRAPH
          END-IF

          DELETE CLIENTE

          IF FS NOT = ZEROS
             MOVE "NAO" TO ERRO-LER-D
             STRING "Atenção! Erro ao remover registro. FS: " FS
             DELIMITED BY SIZE INTO CAMPO-ERRO
             CLOSE CLIENTE
             EXIT PARAGRAPH
          END-IF

          CLOSE CLIENTE

          PERFORM LISTAR.
      *-----------------------------------------------------------------
       EDITAR.

          INITIALIZE CAMPO-ERRO ERRO-LER-D

          OPEN I-O CLIENTE
          PERFORM PREENCHE-CHAVE

          MOVE COD-CLI-D  TO COD-CLI
          READ CLIENTE WITH LOCK

          IF FS = "23"
             MOVE "NAO" TO ERRO-LER-D
             STRING "Atenção! Nenhum registro selecionado. FS: " FS
             DELIMITED BY SIZE INTO CAMPO-ERRO
             CLOSE CLIENTE
             EXIT PARAGRAPH
          END-IF

          IF FS NOT = ZEROS
             MOVE "NAO" TO ERRO-LER-D
             STRING "Atenção! Erro ao ler registro. FS: " FS
             DELIMITED BY SIZE INTO CAMPO-ERRO
             CLOSE CLIENTE
             EXIT PARAGRAPH
          END-IF

          MOVE COD-CLI            TO COD-CLI-D
          MOVE RAZAO-CLI           TO RAZAO-CLI-D
          MOVE CNPJ-CLI            TO CNPJ-CLI-D
          MOVE LAT-CLI            TO LAT-CLI-D
          MOVE LONG-CLI           TO LONG-CLI-D

          CLOSE CLIENTE.
      *-----------------------------------------------------------------
       ADICIONAR.

          INITIALIZE CAMPO-ERRO ERRO-LER-D

          OPEN INPUT CLIENTE
          IF FS NOT = ZEROS
              MOVE "NAO"   TO ERRO-LER-D
              STRING "Atenção! Erro na abertura do arquivo CLIENTE."
              "FS: " FS DELIMITED BY SIZE INTO CAMPO-ERRO
              EXIT PARAGRAPH
          END-IF

          INITIALIZE REG-CLI FS

          PERFORM UNTIL FS = "23" OR = "10"
             ADD 1 TO COD-CLI
             READ CLIENTE WITH IGNORE LOCK
          END-PERFORM
          MOVE COD-CLI    TO COD-CLI-D
          INITIALIZE RAZAO-CLI-D CNPJ-CLI-D LAT-CLI-D LONG-CLI-D
          CLOSE CLIENTE.
      *-----------------------------------------------------------------
       SELECIONAR.
          INITIALIZE REG-CLI
          PERFORM PREENCHE-CHAVE

          OPEN INPUT CLIENTE
          IF FS = "35"
             OPEN OUTPUT CLIENTE
             CLOSE CLIENTE
             OPEN INPUT CLIENTE
          END-IF

          READ CLIENTE WITH IGNORE LOCK

          CLOSE CLIENTE

          PERFORM MOVE-DADOS.
      *-----------------------------------------------------------------
       MOVE-DADOS.
          MOVE COD-CLI            TO COD-CLI-D
          MOVE RAZAO-CLI           TO RAZAO-CLI-D.
      *-----------------------------------------------------------------
       PREENCHE-CHAVE.
          MOVE 03                          TO LVITEM-LENGTH (1)

          MOVE 0                           TO NUMERIC-VALUE
          MOVE "GET-SELECTED-LIST-ITEM"    TO CALL-FUNCTION
          SET OBJECT-REFERENCE             TO WS-LISTA
          CALL "ED-LIST" USING DATA-BLOCK-LV

          IF NUMERIC-VALUE2 IS NUMERIC
          AND NUMERIC-VALUE2 > 0
             MOVE NUMERIC-VALUE2         TO NUMERIC-VALUE
             MOVE "RETRIEVE-LIST-ITEM"   TO CALL-FUNCTION
             SET OBJECT-REFERENCE        TO WS-LISTA
             CALL "ED-LIST" USING DATA-BLOCK-LV
             MOVE FUNCTION NUMVAL(LVITEM-TEXT(1)) TO COD-CLI-D.
      *-----------------------------------------------------------------
       GRAVAR.
          INITIALIZE ERRO-LER-D CAMPO-ERRO REG-CLI

          OPEN I-O CLIENTE
          IF FS NOT = ZEROS
             MOVE "NAO" TO ERRO-LER-D
             STRING "Atenção! Erro na abertura do arquivo CLIENTE. FS: "
             FS DELIMITED BY SIZE INTO CAMPO-ERRO
             CLOSE CLIENTE
             EXIT PARAGRAPH
          END-IF

          MOVE COD-CLI-D      TO COD-CLI
          MOVE RAZAO-CLI-D     TO RAZAO-CLI
          MOVE CNPJ-CLI-D      TO CNPJ-CLI
          MOVE LAT-CLI-D      TO LAT-CLI
          MOVE LONG-CLI-D     TO LONG-CLI

          PERFORM INVOKA-CNPJ
          IF ERRO-LER-D NOT = SPACES
             CLOSE CLIENTE
             EXIT PARAGRAPH
          END-IF

          WRITE REG-CLI

          IF FS = "22"
             REWRITE REG-CLI
             IF FS = "23"
                MOVE "NAO" TO ERRO-LER-D
                STRING "Atenção! Erro CNPJ já cadastado anteriormente. "
                "FS: " FS DELIMITED BY SIZE INTO CAMPO-ERRO
                CLOSE CLIENTE
                EXIT PARAGRAPH
             END-IF
          END-IF

          IF FS NOT = ZEROS
             MOVE "NAO"        TO ERRO-LER-D
             STRING "Atenção! Erro na gravação do registro. FS: "
             FS DELIMITED BY SIZE INTO CAMPO-ERRO
             CLOSE CLIENTE
             EXIT PARAGRAPH
          END-IF

          CLOSE CLIENTE

          PERFORM LISTAR.
      *-----------------------------------------------------------------
       CONFIGURA-SELECAO.

         INITIALIZE WS-STRING-LEN WS-SPACES

         MOVE SPACES TO CAMINHO-CSV-D BUFFER PSZPATH
         MOVE WIN-HANDLE-D TO HWNDOWNER
         SET PSZDISPLAYNAME TO ADDRESS OF BUFFER
         MOVE z"Selecione o endereço" TO USER-TITLE
         SET LPSZTITLE TO ADDRESS OF USER-TITLE
         CALL CC66 "SHBrowseForFolder" USING BROWSE-INFO
                                      RETURNING LPITEMIDLIST
         INSPECT BUFFER REPLACING ALL X"00" BY SPACES
         CALL CC66 "SHGetPathFromIDList" USING BY VALUE LPITEMIDLIST
                                        BY REFERENCE PSZPATH
                                        RETURNING RET-CODE
         INSPECT PSZPATH REPLACING ALL X"00" BY SPACES

         INSPECT FUNCTION REVERSE(PSZPATH) TALLYING WS-SPACES FOR
                                                          LEADING SPACES
         COMPUTE WS-STRING-LEN = LENGTH OF PSZPATH - WS-SPACES

         IF PSZPATH(WS-STRING-LEN:1) NOT = "\"
            ADD 1               TO WS-STRING-LEN
            MOVE "\"            TO PSZPATH(WS-STRING-LEN:1)
         END-IF

         STRING PSZPATH(1:WS-STRING-LEN)
                 "EDPE050-MOVIMENTACOES-PEDIDOS.CSV" INTO CAMINHO-CSV-D.
      *-----------------------------------------------------------------
       LISTAR.
          SET OBJECT-REFERENCE TO WS-LISTA
          MOVE "CLEAR-OBJECT"  TO CALL-FUNCTION
          CALL "ED-LIST" USING DATA-BLOCK-LV

          OPEN INPUT CLIENTE

          INITIALIZE REG-CLI
          START CLIENTE KEY IS >= XAV-CLI
          IF FS EQUAL ZEROS
             READ CLIENTE NEXT WITH IGNORE LOCK
             IF FS EQUAL ZEROS
                PERFORM UNTIL FS EQUAL "10"
                   PERFORM PREENCHE-LINHA
                   MOVE "INSERT-LIST-ITEM" TO CALL-FUNCTION
                   SET OBJECT-REFERENCE    TO WS-LISTA
                   CALL "ED-LIST" USING DATA-BLOCK-LV
                   READ CLIENTE NEXT WITH IGNORE LOCK
                END-PERFORM
             END-IF
          END-IF

          CLOSE CLIENTE.
      *-----------------------------------------------------------------
       IMPORTAR.

          INITIALIZE CAMPO-ERRO ERRO-LER-D WS-COD-CLI

          MOVE CAMINHO-CSV-D       TO WS-NOME-CSV
          OPEN INPUT CSV-FILE

          IF FS NOT = ZEROS
             MOVE "NAO"        TO ERRO-LER-D
             STRING "Atenção! Erro ao abrir arquivo csv. FS: " FS
                           DELIMITED BY SIZE INTO CAMPO-ERRO
             CLOSE CSV-FILE
             EXIT PARAGRAPH
          END-IF

          INITIALIZE REG-CSV
          READ CSV-FILE

          IF IGNORA-CAB-D = 1
             READ CSV-FILE
          END-IF

          PERFORM ADICIONAR
          MOVE COD-CLI        TO WS-COD-CLI

          OPEN I-O CLIENTE
          IF FS NOT = ZEROS
             MOVE "NAO"            TO ERRO-LER-D
             STRING "Atenção! Erro na abertura do arquivo CLIENTE: FS:"
             FS DELIMITED BY SIZE INTO CAMPO-ERRO
             CLOSE CLIENTE CSV-FILE
             EXIT PARAGRAPH
          END-IF

          PERFORM UNTIL FS NOT = ZEROS
             INITIALIZE REG-CLI WS-LAT-EDIT WS-LONG-EDIT WS-LAT-TEXT
             WS-LONG-TEXT CAMPO-ERRO ERRO-LER-D

             MOVE WS-COD-CLI      TO COD-CLI

             UNSTRING LINHA-CSV DELIMITED BY "," INTO
             CNPJ-CLI RAZAO-CLI WS-LAT-TEXT WS-LONG-TEXT

             IF CNPJ-CLI IS NOT NUMERIC OR CNPJ-CLI IS ZEROS
                READ CSV-FILE
                EXIT PERFORM CYCLE
             END-IF

             MOVE FUNCTION NUMVAL(WS-LAT-TEXT) TO WS-LAT-EDIT
             MOVE FUNCTION NUMVAL(WS-LONG-TEXT) TO WS-LONG-EDIT

             MOVE WS-LAT-EDIT          TO LAT-CLI
             MOVE WS-LONG-EDIT         TO LONG-CLI

             MOVE CNPJ-CLI TO CNPJ-CLI-D
             PERFORM INVOKA-CNPJ
             IF ERRO-LER-D NOT = SPACES
                READ CSV-FILE
                INITIALIZE CAMPO-ERRO ERRO-LER-D
                EXIT PERFORM CYCLE
             END-IF

             WRITE REG-CLI
             IF FS NOT = ZEROS
                READ CSV-FILE
                EXIT PERFORM CYCLE
             END-IF

             ADD 1 TO WS-COD-CLI

             READ CSV-FILE
          END-PERFORM

          CLOSE CSV-FILE CLIENTE.
      *-----------------------------------------------------------------
       REMOVER-TODOS.

          INITIALIZE CAMPO-ERRO ERRO-LER-D

          OPEN OUTPUT CLIENTE
          CLOSE CLIENTE.
      *-----------------------------------------------------------------
       INVOKA-CNPJ.
          INITIALIZE ERRO-LER-D CAMPO-ERRO
          invoke EDCNPJ "new" RETURNING OBJ-CNPJ
          invoke OBJ-CNPJ "CNPJ" USING CNPJ-CLI-D
          MOVE SPACE TO ERRO-LER-D
          INVOKE OBJ-CNPJ "GET-MSG-RET" RETURNING CAMPO-ERRO
          IF CAMPO-ERRO NOT = SPACE
             MOVE "NAO" TO ERRO-LER-D
          END-IF.
      *-----------------------------------------------------------------
       PREENCHE-LINHA.
          INITIALIZE LVIEW-ITEM
          MOVE COD-CLI               TO LVITEM-TEXT(1)
          MOVE CNPJ-CLI               TO LVITEM-TEXT(2)
          MOVE RAZAO-CLI              TO LVITEM-TEXT(3)
          MOVE LAT-CLI               TO WS-LAT-EDIT
          MOVE LONG-CLI              TO WS-LONG-EDIT
          MOVE WS-LAT-EDIT            TO LVITEM-TEXT(4)
          MOVE WS-LONG-EDIT           TO LVITEM-TEXT(5).
      *-----------------------------------------------------------------
       CALL-DIALOG-SYSTEM.
          CALL DIALOG-SYSTEM USING DS-CONTROL-BLOCK DATA-BLOCK
          IF DS-ERROR-CODE NOT EQUAL ZEROS
             MOVE DS-ERROR-CODE      TO DISPLAY-ERROR-NO
             DISPLAY "DS ERROR NO:   "        DISPLAY-ERROR-NO
             DISPLAY "ERROR DETAILS(1) :   "  DISPLAY-DETAILS-1
             DISPLAY "ERROR DETAILS(2) :   "  DISPLAY-DETAILS-2
             STOP RUN.
      *-----------------------------------------------------------------

