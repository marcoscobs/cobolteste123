       01 Objetos.

      *** Variaveis usadas por todos os objetos
          02 OO-Global.
             03 OO-Contador                   pic 9(08).
             03 Register                      pic x(04) comp-x.
             03 OO-Config-Flag                pic x(04) comp-5.
             03 OO-Config-Value               pic x(04) comp-5.
             03 OO-Numero-1                   pic x(04) comp-5.
             03 OO-Numero-2                   pic x(04) comp-5.
             03 OO-Funcao                     pic x(30).                Call-Function
             03 OO-Handle-Objeto              pic x(04) comp-x.
             03 OO-Altura-Tela                pic x(04) comp-5.
             03 OO-Largura-Tela               pic x(04) comp-5.
             03 OO-Programa-Lista             pic x(08).
             03 OO-Objeto-Funcao              object reference.         Object-Reference
             03 OO-Window-Principal           object reference.
             03 OO-Endereco-Variavel          usage pointer.

          02 OO-Propriedades.
             03 OO-Arrastar                   pic 9(01) value 1.
                88 P-Arrastar                           value 1 false 0.
             03 OO-Check-Box                  pic 9(01) value 0.
                88 P-Check-Box                          value 1 false 0.
             03 OO-Edicao                     pic 9(01) value 0.
                88 P-Edicao                             value 1 false 0.
             03 OO-Grades                     pic 9(01) value 0.
                88 P-Grades                             value 1 false 0.
             03 OO-Multipla-Selecao           pic 9(01) value 0.
                88 P-Multipla-Selecao                   value 1 false 0.
             03 OO-Ordenar                    pic 9(01) value 0.
                88 P-Ordenar                         value 1 false 0.

      *** Variaveis usadas pelas ListView.
          02 OO-Lista.
             03 LV-Objeto                     usage object reference.
             03 LV-Coluna-Selecionada         pic x(04) comp-5.
             03 LV-Lista-Corrente             pic 9(05).
             03 LV-Num-Colunas                pic 9(09) comp-5.
             03 LV-Num-Itens                  pic 9(09) comp-5.
             03 LV-Ordenar-Coluna             pic 9(09) comp-5.
             03 LV-Selecionado                pic x(04) comp-5.
             03 LV-Vezes-Rolagem              pic 9(05) value zeros.
             03 LV-Atualizar-Ultimo           pic 9   value zero.
                 88 Atualizar-Ultimo          value 1 false 0.

             03 LV-Flag-Alterar               pic 9(01) value 0.
                88 LV-Alterar                           value 1 false 0.
             03 LV-Flag-Avancar               pic 9(01) value 0.
                88 LV-Avancar                           value 1 false 0.
             03 LV-Flag-Fim-Registros         pic 9(01) value 0.
                88 LV-Fim-Registros                     value 1 false 0.
             03 LV-Flag-Iniciar-Lista         pic 9(01) value 0.
                88 LV-Iniciar-Lista                     value 1 false 0.
             03 LV-Flag-Lista-Carregada       pic 9(01) value 0.
                88 LV-Lista-Carregada                   value 1 false 0.
             03 LV-Flag-Mais-de-50            pic 9(01) value 0.
                88 LV-Mais-de-50                        value 1 false 0.
             03 LV-Flag-Uma-Vez-Barra-Cima    pic 9(01) value 0.
                88 LV-Uma-Vez-Barra-Cima                value 1 false 0.
             03 LV-Flag-Uma-Vez-Barra-Baixo   pic 9(01) value 0.
                88 LV-Uma-Vez-Barra-Baixo               value 1 false 0.

      *** Variaveis do Cabeçalho da ListView.
          02 LV-Colunas.
             03 LV-Coluna                     occurs 20.
                04 LV-Alinhamento             pic x(01).                C - Centro, D - Direita, E - Esquerda
                04 LV-Tamanho                 pic x(04) comp-5.
                04 LV-Texto                   pic x(50) value spaces.
                04 LV-Editada                 pic 9(01) value 0.
                   88 LV-P-Editada                      value 1 false 0.
                04 LV-Ordenada                pic 9(01) value 0.
                   88 LV-P-Ordenada                     value 1 false 0.
                04 LV-Objeto-Lista            object reference.

      *** Variaveis das Colunas da ListView
          02 LV-Dados.
             03 LV-Dados-Item                 occurs 50.
                04 LV-Icone                   pic x.
                04 LV-Dado-Objeto             object reference.
                04 LV-Coluna-1                pic x(50).
                04 LV-Coluna-2                pic x(50).
                04 LV-Coluna-3                pic x(50).
                04 LV-Coluna-4                pic x(50).
                04 LV-Coluna-5                pic x(50).
                04 LV-Coluna-6                pic x(50).
                04 LV-Coluna-7                pic x(50).
                04 LV-Coluna-8                pic x(50).
                04 LV-Coluna-9                pic x(50).
                04 LV-Coluna-10               pic x(50).
                04 LV-Coluna-11               pic x(50).
                04 LV-Coluna-12               pic x(50).
                04 LV-Coluna-13               pic x(50).
                04 LV-Coluna-14               pic x(50).
                04 LV-Coluna-15               pic x(50).
                04 LV-Coluna-16               pic x(50).
                04 LV-Coluna-17               pic x(50).
                04 LV-Coluna-18               pic x(50).
                04 LV-Coluna-19               pic x(50).
                04 LV-Coluna-20               pic x(50).

          02 LVDRAG-IN-PROGRESS               PIC x COMP-X.

          02 TP-Grupo-Tooltip.
             03 TP-Tooltip                    occurs 200.
                04 TP-Handle-Objeto           pic x(04) comp-x.
                04 TP-Texto                   pic x(256).
      /         04 TP-Linhas                  pic 9(01) value 0.        Propriedade para colocar mais de uma linha no ToolTip(futuramente).
      /            88 LV-P-Linhas                       value 1 false 0.

          02 OO-Funcoes.
             04 Fu-Campos                     pic  x(1024).
             04 Fu-Codigo                     pic S9(08)   value zeros.
             04 Fu-Conteudo-Chv               pic  x(256)  value spaces.
             04 Fu-Conteudo-Pos               pic  x(4096) value spaces.
             04 Fu-Descricao                  pic  x(40)   value spaces.
             04 Fu-Estado                     pic  x(02).
             04 Fu-Handle-Objeto              pic  x(04) comp-x.
             04 Fu-Texto                      pic  x(270).
             04 Fu-Tamanho                    pic  xx    comp-5.
             04 Fu-Condicao                   pic  x(1256) value spaces.


             04 Filler                        pic 9(01) value 0.
                88 Fu-Executar                          value 1 false 0.

      *** Variáveis para Controle de Encerramento e SqlCode do Programa
          02 SqlCodeAnt                       pic s9(9) comp-5 value 0.

      *** Para avisar sobre a interrupção do processo, quando
      *   pré-estabelecida
          02 Sql-Encerra                      pic 9(01) value 0.
             88 Msg-Encerra                   value 1 false 0.
             88 Prog-Encerra                  value 1 false 0.

          02 OO-Excel.
             03 Ex-Empresa                    pic x(50).
             03 Ex-Nome-Tabela                pic x(128).
             03 Ex-Titulo                     pic x(50).
             03 Ex-Colunas.
                04 Ex-Grupo-Colunas           occurs 15.
                   05 Ex-Alinhamento          pic x(01).                C - Centro, D - Direita, E - Esquerda
                   05 Ex-Nome-Coluna          pic x(50).
                   05 Ex-Tamanho-Coluna       pic 9(05).

             03 Ex-Dados.
                04 Ex-Grupo-Dados             occurs 50.
                   05 Ex-Coluna-1             pic x(50).
                   05 Ex-Coluna-2             pic x(50).
                   05 Ex-Coluna-3             pic x(50).
                   05 Ex-Coluna-4             pic x(50).
                   05 Ex-Coluna-5             pic x(50).
                   05 Ex-Coluna-6             pic x(50).
                   05 Ex-Coluna-7             pic x(50).
                   05 Ex-Coluna-8             pic x(50).
                   05 Ex-Coluna-9             pic x(50).
                   05 Ex-Coluna-10            pic x(50).
                   05 Ex-Coluna-11            pic x(50).
                   05 Ex-Coluna-12            pic x(50).
                   05 Ex-Coluna-13            pic x(50).
                   05 Ex-Coluna-14            pic x(50).
                   05 Ex-Coluna-15            pic x(50).

      *** Funções OO.
       78 Funcao-Alterar-Item              value "ALTERAR".
       78 Funcao-Adicionar-Colunas         value "ADICIONAR".
       78 Funcao-Cor                       value "COR".
       78 Funcao-Deletar-Item              value "DELETAR".
       78 Funcao-Fonte                     value "FONTE".
       78 Funcao-Inserir-Item              value "INSERIR-ITEM".
       78 Funcao-Inserir-Itens             value "INSERIR-ITENS".
       78 Funcao-Item-Selecionado          value "ITEM-SELECIONADO".
       78 Funcao-Limpar-Lista              value "LIMPAR".
       78 Funcao-Pegar-Item                value "PEGAR".
       78 Funcao-Pegar-CheckBox            value "GET-CHECK-BOX-STATE".
       78 Funcao-Selecionar                value "SELECIONAR".
       78 Funcao-Setar-CheckBox            value "SET-CHECK-BOX-STATE".
       78 Funcao-Topo                      value "TOPO".
