      $IF SYS-CONSTANTS NOT DEFINED
       01  SYS-CONSTANTS.
           03  SYS-NULL       PIC 9(8) COMP-X VALUE H"01000000".
           03  SYS-CONTROL    PIC 9(8) COMP-X VALUE H"02000000".
           03  SYS-WINDOW     PIC 9(8) COMP-X VALUE H"03000000".
      $END

       01  filler.

       03  WIN-CLIENTE                     PIC 9(8) COMP-X VALUE 1.
       03  EF-PESQ                         PIC 9(8) COMP-X VALUE 2.
       03  GBOX1                           PIC 9(8) COMP-X VALUE 3.
       03  PB-ADD                          PIC 9(8) COMP-X VALUE 4.
       03  GBOX7                           PIC 9(8) COMP-X VALUE 5.
       03  USER1                           PIC 9(8) COMP-X VALUE 6.
       03  PB-IMPORT                       PIC 9(8) COMP-X VALUE 7.
       03  PB-EDIT                         PIC 9(8) COMP-X VALUE 8.
       03  PB1                             PIC 9(8) COMP-X VALUE 9.
       03  PB-REMOVER-TODOS                PIC 9(8) COMP-X VALUE 10.
       03  DBOX-AGUARDE                    PIC 9(8) COMP-X VALUE 11.
       03  GBOX9                           PIC 9(8) COMP-X VALUE 12.
       03  MBOX-ERRO                       PIC 9(8) COMP-X VALUE 13.
       03  MBOX-CONF                       PIC 9(8) COMP-X VALUE 14.
       03  WIN-TABELA                      PIC 9(8) COMP-X VALUE 15.
       03  LB-TABELA                       PIC 9(8) COMP-X VALUE 16.
       03  GBOX10                          PIC 9(8) COMP-X VALUE 17.
       03  PB-OK-TABELA                    PIC 9(8) COMP-X VALUE 18.
       03  MBOX-ALER                       PIC 9(8) COMP-X VALUE 19.
       03  DBOX-CLI                        PIC 9(8) COMP-X VALUE 20.
       03  EF-COD-VEND                     PIC 9(8) COMP-X VALUE 21.
       03  EF-NOME-VEND                    PIC 9(8) COMP-X VALUE 22.
       03  EF-CPF-VEND                     PIC 9(8) COMP-X VALUE 23.
       03  EF-LAT-VEND                     PIC 9(8) COMP-X VALUE 24.
       03  EF-LONG-VEND                    PIC 9(8) COMP-X VALUE 25.
       03  PB-GRAVAR                       PIC 9(8) COMP-X VALUE 26.
       03  DBOX-IMPORT                     PIC 9(8) COMP-X VALUE 27.
       03  EF-CAMINHO-CSV                  PIC 9(8) COMP-X VALUE 28.
       03  PB6                             PIC 9(8) COMP-X VALUE 29.
       03  PB-GRAVA-CFG                    PIC 9(8) COMP-X VALUE 30.
       03  CB-GNORA-CAB                    PIC 9(8) COMP-X VALUE 31.
