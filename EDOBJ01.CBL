      $set nodb2
       class-id. edobj01 data is private
           inherits from Base.

      *******************************************************************
      * Programa       : EdObj01.cbl renomeado de SG117001              *
      * Data de escrita: 10/04/2006                                     *
      * Programador    : Anderson de Freitas  - AMPLA                   *
      *                                                                 *
      * Alterado: 03/2009 - 04/2009                                     *
      * Programador: Marcos Soares  - EDIL SYSTEM                       *
      * Principais alterações: Adaptado para funcionar no NetExpress 3.1*
      *                        Metodo CentralizarTela Reescrito         *
      *                        Acrescentados eventos de Teclas          *
      *                        Metodo CriarMensagem Reescrito           *
      * Implementações:                                                 *
      *                 Metodo Pesquisar                                *
      *******************************************************************
      * Descrição: Classe intermediaria entre o programa que tem        *
      *            listview e o programa que controla todas as listviews*
      *            do sistema.                                          *
      *******************************************************************
      * Ordem dos tipos de métodos: 1 - Propriedades                    *
      *                             2 - Manutenção                      *
      *                             3 - Atualização do Copy             *
      *                             4 - Edição                          *
      *                             5 - Barras de Rolagem               *
      *                             6 - Controle Primeiro e Último      *
      *                             7 - Internos                        *
      *                             8 - Métodos Gerais do Sistema       *
      *******************************************************************

       environment division.
       special-names.
           call-convention 74 is cc74.

       object section.

       class-control.
           EdObj01         is class "edobj01"
           SG050001        is class "sg050001"
           Base            is class "base"
      *CharacterArray--------------------------
           Strings         is class "chararry"
      *----------------------------------------
           dialogbox       is class "dilogbox"
           Dependent       is class "dependnt"
           EventManager    is class "p2emgr"
      *messagebox------------------------------
           messagebox      is class "msgbox"
      *----------------------------------------
           SysInformation  is class "sysinfo"
           Tooltip         is class "tooltip"
           Window          is class "window"

           CharArray       is class 'chararry'
           CharacterArray  is class 'chararry'
           Filename        is class 'filename'
           IconData        is class 'icondata'
           Colection       is class 'ordrdcll'.
      *-----------------------------------------------------------------
       data division.

       working-storage section.

      *** Gerais.
       01 Indice                            pic  x(04) comp-5.
       01 Indice-2                          pic  x(04) comp-5.
       01 Item                              pic  x(1000) value spaces.
       01 Altura-Objeto                     pic s9(09) comp-5.
       01 Largura-Objeto                    pic s9(09) comp-5.
       01 Handle-Objeto                     pic  x(04) comp-5.
       01 aObjeto                           object reference value null.
       01 aWindow                           object reference value null.
       01 aWindowMain                       object reference value null.
       01 aMenu                             object reference value null.
       01 aDesktop                          object reference.
       01 aTexto                            object reference value null.
       01 aTitulo                           object reference value null.

      *** Lista.
       01 Lista-Atual                       object reference value null.
       01 Lista-Anterior                    object reference value null.
       01 Lista-Corrente                    pic 9(05) value zeros.
       01 Num-Entre-Listas                  pic 9(05) value zeros.
       01 pesq                              pic x(139).
       01 pesq-ant                          pic x(139).

       01 Flag-Fim-Laco                     pic 9(01) value zero.
          88 Fim-Laco                                 value 1 false 0.
       01 Flag-Alterar-Linha                pic 9(01) value zero.
          88 Alterar-Linha                            value 1 false 0.
      *******************************************************************
       77 w-NomeCompleto       pic x(256) value spaces.
       77 w-caminho            pic x(256) value spaces.
       77 umaColecao           object reference value null.
       77 umaMascara           object reference value null.
       77 umArquivo            object reference value null.
       77 umNomeArq            object reference value null.
       77 umCaminho            object reference value null.
       77 lvSize               pic 9(09) comp-5 value zeros.
       77 lvLoop               pic 9(09) comp-5 value zeros.
       77 wTam                 pic s9(9) comp-5 value zeros.
      *******************************************************************
      *                         LISTAS E CONTROLES.                     *
      *******************************************************************
       01 Todas-Listas                      occurs 100.
          02 aLista                         object reference value null.
          02 Programa-Lista                 pic x(08) value spaces.
          02 Endereco-Copy                  usage pointer.

      *** Auxiliares
          02 Auxiliares.
             03 Aux-Num-Colunas             pic  x     comp-5.
             03 Aux-Num-Itens               pic  9(05) value zeros.

             03 Aux-Colunas                 occurs 20.
                04 Aux-Largura              pic x(04) comp-5.
                04 Aux-Posicao              pic x(04) comp-5.
                04 Aux-Tamanho              pic x(04) comp-5.
                04 Aux-Editada              pic 9(01) value 0.
                   88 Aux-P-Editada                   value 1 false 0.
                04 Aux-Ordenada             pic 9(01) value 0.
                   88 Aux-P-Ordenada                  value 1 false 0.

             03 Aux-Barra-Vertical.
                04 Aux-Vezes-Rolagem        pic  9(05) value zeros.
                04 Aux-Menor-Cima           pic s9(09) comp-5.
                04 Aux-Maior-Cima           pic s9(09) comp-5.
                04 Aux-Menor-Baixo          pic s9(09) comp-5.
                04 Aux-Maior-Baixo          pic s9(09) comp-5.

             03 aPrimeiro                   object reference.
             03 aUltimo                     object reference.

          02 Flags-Lista.
             03 Flag-Avancar                pic 9(01) value 0.
                88 Avancar                            value 1 false 0.
             03 Flag-Fim-Registros          pic 9(01) value 0.
                88 Fim-Registros                      value 1 false 0.
             03 Flag-Lista-Carregada        pic 9(01) value zero.
                88 Lista-Carregada                    value 1 false 0.
             03 Flag-Inicializada           pic 9(01) value zero.
                88 Lista-Inicializada                 value 1 false 0.
             03 Flag-Mais-de-50             pic 9(01) value 0.
                88 Mais-de-50                         value 1 false 0.
             03 Flag-Secundaria             pic 9(01) value zero.
                88 Lista-Secundaria                   value 1 false 0.
             03 Flag-Setas-Barra            pic 9(01) value 0.
                88 Setas-Barra                        value 1 false 0.
             03 Flag-Uma-Vez-Barra-Cima     pic 9(01) value 0.
                88 Uma-Vez-Barra-Cima                 value 1 false 0.
             03 Flag-Uma-Vez-Barra-Baixo    pic 9(01) value 0.
                88 Uma-Vez-Barra-Baixo                value 1 false 0.

      *******************************************************************
      *                         LISTAS E CONTROLES.                     *
      *******************************************************************

       class-object.
       object-storage section.

      *-----------------------------------------------------------------

      *******************************************************************
      *                          PROPRIEDADES                           *
      *******************************************************************
       method-id. "AdicionarColunas".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.
      **** Conta Quantas colunas tem.
           initialize OO-Lista LV-Ordenar-Coluna

           perform varying Indice from 1 by 1 until Indice > 20 or
                                        LV-Texto  (Indice) = spaces
               add 1 to LV-Num-Colunas
           end-perform

      **** Prepara a List View.
           set Fim-Laco to false
           perform varying Indice from 1 by 1 until Indice > 100 or
                                                           Fim-Laco
MARCOS         IF aLista (Indice) = NULL
MARCOS            ADD 1 TO INDICE
MARCOS         END-IF

               if not  Lista-Inicializada(Indice)
                  set  Lista-Inicializada(Indice) to true
                  set  Fim-Laco          to true
                  set  LV-Objeto         to         aLista (Indice)
                  set  Lista-Atual       to         aLista (Indice)
                  move OO-Programa-Lista to Programa-Lista (Indice)
                  move LV-Num-Colunas    to Aux-Num-Colunas(Indice)

      **** Dados de edicao.
                  perform varying Indice-2 from 1 by 1 until Indice-2
                                                     > LV-Num-Colunas
                      move LV-Tamanho  (       Indice-2)
                        to Aux-Tamanho (Indice,Indice-2)
                      move LV-Editada  (       Indice-2)
                        to Aux-Editada (Indice,Indice-2)
                      move LV-Ordenada (       Indice-2)
                        to Aux-Ordenada(Indice,Indice-2)
                  end-perform
                  invoke self "Propriedades" using Objetos
               end-if
           end-perform

      **** Define o tamanho de colunas.
           move Funcao-Adicionar-Colunas to OO-Funcao
           call        "EDLIST01"     using Objetos
           invoke self "LimparLista"  using Objetos

           initialize OO-Propriedades LV-Flag-Alterar LV-Colunas

       end method "AdicionarColunas".
      *-----------------------------------------------------------------

       method-id. "Propriedades".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.
      **** Metodo que permite que as colunas sejam arrastadas.
           if P-Arrastar
              invoke LV-Objeto "HeaderDragDrop"
           end-if

      **** Metodo que insere Ckeck Box.
           if P-Check-box
              invoke LV-Objeto "CheckBoxes"
           end-if

      **** Metodo que habilita edição na listview.
           if P-Edicao
              invoke LV-Objeto "allowItemsToBeEdited"
           end-if

      **** Metodo que habilita grades.
           if P-Grades
              invoke LV-Objeto "GridLines"
           end-if

           if P-Multipla-Selecao
              invoke LV-Objeto "MultipleSelection"
           end-if

      **** Metodo que ordena as colunas quando clicadas.
           if P-Ordenar
              invoke LV-Objeto "SortHeader"
           else
              invoke LV-Objeto "NoSortHeader"
           end-if

       end method "Propriedades".
      *-----------------------------------------------------------------

       method-id. "Selecao".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           move Funcao-Selecionar to OO-Funcao
           call   "EDLIST01"   using Objetos

           MOVE "set-focus"       to OO-Funcao
           call   "EDLIST01"   using Objetos

       end method "Selecao".
      *-----------------------------------------------------------------

       method-id. "VoltarAnterior".

       procedure division.

           set Lista-Atual to Lista-Anterior

       end method "VoltarAnterior".
      *-----------------------------------------------------------------

      *******************************************************************
      *                           MANUTENCAO                            *
      *******************************************************************
       method-id. "AdicionarItem".
       01 Tamanho-Lista  pic x(04) comp-5.

       01 Texto          pic  x(1000).
       01 I              pic x(04) comp-5.

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.
           invoke LV-Objeto "size" returning Tamanho-Lista

           if Tamanho-Lista = zeros
              move LV-Texto( 1) to LV-Coluna-1(1)
              move LV-Texto( 2) to LV-Coluna-2(1)
              move LV-Texto( 3) to LV-Coluna-3(1)
              move LV-Texto( 4) to LV-Coluna-4(1)
              move LV-Texto( 5) to LV-Coluna-5(1)
              move LV-Texto( 6) to LV-Coluna-6(1)
              move LV-Texto( 7) to LV-Coluna-7(1)
              move LV-Texto( 8) to LV-Coluna-8(1)
              move LV-Texto( 9) to LV-Coluna-9(1)
              move LV-Texto(10) to LV-Coluna-10(1)
              move LV-Texto(11) to LV-Coluna-11(1)
              move LV-Texto(12) to LV-Coluna-12(1)
              move LV-Texto(13) to LV-Coluna-13(1)
              move LV-Texto(14) to LV-Coluna-14(1)
              move LV-Texto(15) to LV-Coluna-15(1)
              move LV-Texto(16) to LV-Coluna-16(1)
              move LV-Texto(17) to LV-Coluna-17(1)
              move LV-Texto(18) to LV-Coluna-18(1)
              move LV-Texto(19) to LV-Coluna-19(1)
              move LV-Texto(20) to LV-Coluna-20(1)
              move 1            to LV-Num-Itens
              invoke self "AdicionarItens" using Objetos

              exit method
           end-if

           invoke self "AtualizarValLista" using Objetos
           move   Funcao-Inserir-Item   to    OO-Funcao
           call        "EDLIST01"       using Objetos
           *> Insere o item na coleção
           string LV-Texto(1 ) LV-Texto(2 )
                  LV-Texto(3 ) LV-Texto(4 )
                  LV-Texto(5 ) LV-Texto(6 )
                  LV-Texto(7 ) LV-Texto(8 )
                  LV-Texto(9 ) LV-Texto(10)
                  LV-Texto(11) LV-Texto(12)
                  LV-Texto(13) LV-Texto(14)
                  LV-Texto(15) LV-Texto(16)
                  LV-Texto(17) LV-Texto(18)
                  LV-Texto(19) LV-Texto(20) into Texto

           move   1000 to I
           invoke Strings "withByteLengthValue"    using I Texto
                                                   returning  aTexto

           invoke aUltimo(Num-Entre-Listas)   "add" using aTexto

           if LV-Ordenar-Coluna not = zeros
              invoke LV-Objeto "sortOnColumn" using LV-Ordenar-Coluna
           end-if

           move zeros to LV-Ordenar-Coluna

       end method "AdicionarItem".
      *-----------------------------------------------------------------

       method-id. "AdicionarItens".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

      *    invoke SG050001 "Pegar50Primeiros"  using Objetos
           invoke self     "AtualizarValLista" using Objetos

           if LV-Num-Itens not = zeros
              move   LV-Num-Itens to Aux-Num-Itens(Num-Entre-Listas)

              set    Lista-Atual              to    LV-Objeto
              invoke self "LimparLista"       using Objetos

              move   Funcao-Inserir-Itens     to    OO-Funcao
              call        "EDLIST01"          using Objetos
           else
              if Avancar(Num-Entre-Listas)
                 subtract 1 from Aux-Vezes-Rolagem(Num-Entre-Listas)
              else
                 add      1   to Aux-Vezes-Rolagem(Num-Entre-Listas)
              end-if
           end-if

       end method "AdicionarItens".
      *-----------------------------------------------------------------
       method-id. "AlterarItem-AMPLA".
       77 i                            pic x(04) comp-5.
       77 texto                        pic x(1000).
       77 atexto2                      object reference.

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           set    LV-Alterar to true
           invoke EdObj01 "AtualizarValLista" using Objetos
           move   LV-Selecionado      to OO-Numero-1
           move   Funcao-Alterar-Item to OO-Funcao
           call  "EDLIST01"        using Objetos
           set    LV-Alterar          to false

           invoke aUltimo(Num-Entre-Listas) "at" using LV-Selecionado
                                                 returning aTexto

           initialize texto
           string LV-Texto(1 ) LV-Texto(2 )
                  LV-Texto(3 ) LV-Texto(4 )
                  LV-Texto(5 ) LV-Texto(6 )
                  LV-Texto(7 ) LV-Texto(8 )
                  LV-Texto(9 ) LV-Texto(10)
                  LV-Texto(11) LV-Texto(12)
                  LV-Texto(13) LV-Texto(14)
                  LV-Texto(15) LV-Texto(16)
                  LV-Texto(17) LV-Texto(18)
                  LV-Texto(19) LV-Texto(20) into Texto

           move   1000 to I
           invoke Strings "withByteLengthValue" using I Texto
                                                   returning aTexto2

           invoke aTexto "resetValue" using aTexto2

           invoke aTexto2 "finalize" returning aTexto2
       end method "AlterarItem-AMPLA".
      *=================================================================


       method-id. "AlterarItem".
       77 i                            pic x(04) comp-5.
       77 texto                        pic x(1000).
       77 atexto2                      object reference.

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           set    Lv-Alterar          to true
           move   Funcao-Alterar-Item to OO-Funcao
           call  "EDLIST01"        using Objetos
           set    LV-Alterar          to false

       end method "AlterarItem".
      *-----------------------------------------------------------------

       method-id. "PegarStatusCheckBox".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           set  LV-Objeto      to Lista-Atual
           set  LV-Alterar     to false
           move Lista-Corrente to LV-Lista-Corrente

      **** O valor de OO-Numero-1 tem que ser 0 para a pesquisa do item
      *    selecionado começar do primeiro item da lista.
           move 0                       to OO-Numero-1
           move Funcao-Item-Selecionado to OO-Funcao
           call      "EDLIST01"      using Objetos

           move LV-Selecionado          to OO-Numero-1
           move Funcao-Pegar-CheckBox   to OO-Funcao
           call      "EDLIST01"      using Objetos

       end method "PegarStatusCheckBox".
      *-----------------------------------------------------------------
       method-id. "SetarStatusCheckBox".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           set  LV-Objeto      to Lista-Atual
           set  LV-Alterar     to false
           move Lista-Corrente to LV-Lista-Corrente

      **** O valor de OO-Numero-1 tem que ser 0 para a pesquisa do item
      *    selecionado começar do primeiro item da lista.
           move 0                       to OO-Numero-1
           move Funcao-Item-Selecionado to OO-Funcao
           call      "EDLIST01"      using Objetos

           move LV-Selecionado          to OO-Numero-1
           move Funcao-Setar-CheckBox   to OO-Funcao
           call      "EDLIST01"      using Objetos

       end method "SetarStatusCheckBox".
      *-----------------------------------------------------------------
       method-id. "MarcarTodosCheckBox".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           set  LV-Objeto      to Lista-Atual
           set  LV-Alterar     to false
           move Lista-Corrente to LV-Lista-Corrente

      **** O valor de OO-Numero-1 tem que ser 0 para a pesquisa do item
      *    selecionado começar do primeiro item da lista.
           move 0                       to OO-Numero-1
           move Funcao-Item-Selecionado to OO-Funcao
           call      "EDLIST01"      using Objetos

           move LV-Selecionado          to OO-Numero-1
           move Funcao-Setar-CheckBox   to OO-Funcao
           call      "EDLIST01"      using Objetos

       end method "MarcarTodosCheckBox".
      *-----------------------------------------------------------------
       method-id. "DesmarcarTodosCheckBox".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           set  LV-Objeto      to Lista-Atual
           set  LV-Alterar     to false
           move Lista-Corrente to LV-Lista-Corrente

      **** O valor de OO-Numero-1 tem que ser 0 para a pesquisa do item
      *    selecionado começar do primeiro item da lista.
           move 0                       to OO-Numero-1
           move Funcao-Item-Selecionado to OO-Funcao
           call      "EDLIST01"      using Objetos

           move LV-Selecionado          to OO-Numero-1
           move Funcao-Setar-CheckBox   to OO-Funcao
           call      "EDLIST01"      using Objetos

       end method "DesmarcarTodosCheckBox".
      *-----------------------------------------------------------------


       method-id. "DeletarItem".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           invoke EdObj01 "AtualizarValLista" using Objetos
           move   LV-Selecionado      to OO-Numero-1
           move   Funcao-Deletar-Item to OO-Funcao
           call  "EDLIST01"        using Objetos

       end method "DeletarItem".
      *-----------------------------------------------------------------

       method-id. "ItemSelecionado".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

MARCOS* O lv-objeto deve ser movido por quem invoka o metodo
MARCOS*    set  LV-Objeto      to Lista-Atual
      *-----------------------------------------------------

           set  LV-Alterar     to false
           move Lista-Corrente to LV-Lista-Corrente

      **** O valor de OO-Numero-1 tem que ser 0 para a pesquisa do item
      *    selecionado começar do primeiro item da lista.
           move 0                       to OO-Numero-1
           move Funcao-Item-Selecionado to OO-Funcao
           call      "EDLIST01"      using Objetos

           move LV-Selecionado          to OO-Numero-1
           move Funcao-Pegar-Item       to OO-Funcao
           call      "EDLIST01"      using Objetos


       end method "ItemSelecionado".
      *-----------------------------------------------------------------

       method-id. "LimparLista".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           set lista-atual to lv-objeto
           invoke Lista-Atual "deleteAll"

       end method "LimparLista".
      *-----------------------------------------------------------------

       method-id. "PegarItem".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           invoke EdObj01 "AtualizarValLista" using Objetos
           move   Funcao-Pegar-Item to OO-Funcao
           call  "EDLIST01"      using Objetos

       end method "PegarItem".
      *-----------------------------------------------------------------

       method-id. "PegarLista".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           invoke EdObj01 "AtualizarValLista" using Objetos
           set    Lista-Atual to LV-Objeto

       end method "PegarLista".
      *-----------------------------------------------------------------

      *******************************************************************
      *                      ATUALIZACAO DO COPY                        *
      *******************************************************************
       method-id. "ListaCorrente".
       working-storage section.
       01 Programa                 pic x(08) value spaces.

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           set Fim-Laco to false
           initialize Programa
MARCOS*    perform varying Indice from 1 by 1 until Indice > 100 or
MARCOS*                 Programa-Lista(Indice) not= spaces or Fim-Laco

MARCOS     perform varying Indice from 1 by 1 until Indice > 100 or
MARCOS                     Fim-Laco

MARCOS         if Programa-Lista(Indice) not= spaces
                   move Indice to Num-Entre-Listas

                   if Programa not = Programa-Lista(Indice)
                       move Programa-Lista(Indice) to Programa
                       move 1                      to Lista-Corrente
                   end-if
                   if LV-Objeto =  aLista(Indice)
                       set  Fim-Laco               to true
                       set  Lista-Atual            to LV-Objeto
                   else
                       add  1                      to Lista-Corrente
                   end-if
MARCOS         end-if
           end-perform

       end method "ListaCorrente".
      *-----------------------------------------------------------------

       method-id. "NumeroItensLista".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           set Fim-Laco to false
           perform varying Indice from 1 by 1 until Indice > 100 or
                                                           Fim-Laco
               move Indice to Num-Entre-Listas

               if LV-Objeto = aLista (Indice)
                  set  Fim-Laco  to true
                  move Programa-Lista(Indice) to OO-Programa-Lista
                  move Aux-Num-Itens (Indice) to LV-Num-Itens
                  move Indice                 to Lista-Corrente
               end-if

           end-perform

       end method "NumeroItensLista".
      *-----------------------------------------------------------------

       method-id. "PegarEndCopy".

       working-storage section.
       copy "EdObj01.cpy".

       linkage section.
       01 aControl        object reference.
       01 EnderecoCPY     usage pointer.

       procedure division using aControl EnderecoCPY.

           set    LV-Objeto to aControl
           invoke EdObj01 "ListaCorrente" using Objetos
           move   Endereco-Copy(Num-Entre-Listas) to EnderecoCPY

       end method "PegarEndCopy".
      *-----------------------------------------------------------------

      *******************************************************************
      *                             EDICAO                              *
      *******************************************************************
       method-id. "GuardarValoresColunas".

       linkage section.
       copy "EdObj01.cpy".
       01 Grupo-Colunas.
          02 Num-Colunas        pic x     comp-5.
          02 Colunas            occurs 20.
             03 Aux-Tamanho     pic x(04) comp-5.
             03 Editada         pic 9(01).
             03 Largura         pic x(04) comp-5.
             03 Posicao         pic x(04) comp-5.

       procedure division using Objetos Grupo-Colunas.

           invoke self "ListaCorrente" using Objetos

           perform varying Indice from 1 by 1 until Indice > Num-Colunas
               move Largura    (                 Indice)
                 to Aux-Largura(Num-Entre-Listas,Indice)
               move Posicao    (                 Indice)
                 to Aux-Posicao(Num-Entre-Listas,Indice)
           end-perform

           move OO-Endereco-Variavel to Endereco-Copy(Num-Entre-Listas)

       end method "GuardarValoresColunas".
      *-----------------------------------------------------------------

       method-id. "HabilitarAlteracaoLinha".
       procedure division.

           set Alterar-Linha to true

       end method "HabilitarAlteracaoLinha".
      *-----------------------------------------------------------------

       method-id. "PegarValoresColunas".

       linkage section.
       copy "EdObj01.cpy".

       01 aReferencia           object reference.
       01 Grupo-Colunas.
          02 Num-Colunas        pic x     comp-5.
          02 Colunas            occurs 20.
             03 Tamanho         pic x(04) comp-5.
             03 Editada         pic 9(01).
             03 Largura         pic x(04) comp-5.
             03 Posicao         pic x(04) comp-5.

       procedure division using aReferencia Grupo-Colunas Objetos.

           initialize LV-Colunas
           set Alterar-Linha Fim-Laco to false

           perform varying Indice from 1 by 1 until Indice > 100 or
                                                           Fim-Laco

               if aReferencia = aLista(Indice)
                  set  Fim-Laco                to true
                  move Aux-Num-Colunas(Indice) to Num-Colunas

                  perform varying Indice-2 from 1 by 1 until Indice-2
                                                           > Num-Colunas
                      move Aux-Tamanho (Indice,Indice-2)
                        to Tamanho     (       Indice-2)
                           LV-Tamanho  (       Indice-2)
                      move Aux-Editada (Indice,Indice-2)
                        to Editada     (       Indice-2)
                      move Aux-Largura (Indice,Indice-2)
                        to Largura     (       Indice-2)
                      move Aux-Posicao (Indice,Indice-2)
                        to Posicao     (       Indice-2)
                  end-perform

               end-if
           end-perform

       end method "PegarValoresColunas".
      *-----------------------------------------------------------------

       method-id. "VerificarAlteracao".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           if Alterar-Linha
              set  LV-Alterar     to true
              move Lista-Corrente to LV-Lista-Corrente
           else
              set  LV-Alterar     to false
           end-if

           set     Alterar-Linha  to false

       end method "VerificarAlteracao".
      *-----------------------------------------------------------------

      *******************************************************************
      *                        BARRAS DE ROLAGEM                        *
      *******************************************************************
       method-id. "AtualizarRolagem".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.


           invoke self "ListaCorrente" using Objetos

           move LV-Flag-Avancar to Flag-Avancar(Num-Entre-Listas)
           move LV-Flag-Fim-Registros
             to    Flag-Fim-Registros          (Num-Entre-Listas)
           move LV-Flag-Uma-Vez-Barra-Baixo
             to    Flag-Uma-Vez-Barra-Baixo    (Num-Entre-Listas)
           move LV-Flag-Uma-Vez-Barra-Cima
             to    Flag-Uma-Vez-Barra-Cima     (Num-Entre-Listas)
           move  LV-Vezes-Rolagem
             to Aux-Vezes-Rolagem              (Num-Entre-Listas)
           move  LV-Vezes-Rolagem
             to Aux-Vezes-Rolagem              (Num-Entre-Listas)

       end method "AtualizarRolagem".
      *-----------------------------------------------------------------

       method-id. "InicioRolagem".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           if LV-Iniciar-Lista
              set  LV-Iniciar-Lista                  to false
              set  Fim-Registros  (Num-Entre-Listas) to false
              set  Lista-Carregada(Num-Entre-Listas) to false
              set  Mais-de-50     (Num-Entre-Listas) to false
              set  Avancar        (Num-Entre-Listas) to false
              initialize Aux-Vezes-Rolagem(Num-Entre-Listas)
                         LV-Vezes-Rolagem
           end-if

           move Flag-Avancar      (Num-Entre-Listas) to LV-Flag-Avancar
           move Flag-Fim-Registros  (Num-Entre-Listas)
             to LV-Flag-Fim-Registros
           move Flag-Lista-Carregada(Num-Entre-Listas)
             to LV-Flag-Lista-Carregada
           move Flag-Mais-de-50     (Num-Entre-Listas)
             to LV-Flag-Mais-de-50
           move Aux-Vezes-Rolagem   (Num-Entre-Listas)
             to LV-Vezes-Rolagem
      /    move Aux-Num-Itens       (Num-Entre-Listas) to

       end method "InicioRolagem".
      *-----------------------------------------------------------------

       method-id. "ValoresBarra".
       working-storage section.
       01 Programa                 pic x(08).

       linkage section.
       copy "EdObj01.cpy".

       01 Barra-Vertical.
          02 Menor-Cima            pic s9(09) comp-5 value zeros.
          02 Maior-Cima            pic s9(09) comp-5 value zeros.
          02 Menor-Baixo           pic s9(09) comp-5 value zeros.
          02 Maior-Baixo           pic s9(09) comp-5 value zeros.

       procedure division using Objetos Barra-Vertical.

           invoke self "ListaCorrente" using Objetos

           move Programa-Lista   (Num-Entre-Listas) to OO-Programa-Lista
           move Aux-Num-Itens    (Num-Entre-Listas) to LV-Num-Itens
           move Aux-Menor-Cima   (Num-Entre-Listas) to Menor-Cima
           move Aux-Maior-Cima   (Num-Entre-Listas) to Maior-Cima
           move Aux-Menor-Baixo  (Num-Entre-Listas) to Menor-Baixo
           move Aux-Maior-Baixo  (Num-Entre-Listas) to Maior-Baixo
           move Aux-Vezes-Rolagem(Num-Entre-Listas) to LV-Vezes-Rolagem

      **** Flags.
           move    Flag-Avancar  (Num-Entre-Listas) to LV-Flag-Avancar
           move    Flag-Fim-Registros      (Num-Entre-Listas)
             to LV-Flag-Fim-Registros
           move Flag-Avancar     (Num-Entre-Listas) to LV-Flag-Avancar
           move    Flag-Uma-Vez-Barra-Cima (Num-Entre-Listas)
             to LV-Flag-Uma-Vez-Barra-Cima
           move    Flag-Uma-Vez-Barra-Baixo(Num-Entre-Listas)
             to LV-Flag-Uma-Vez-Barra-Baixo

       end method "ValoresBarra".
      *-----------------------------------------------------------------

      *******************************************************************
      *                  CONTROLE DE PRIMEIRO E ULTIMO                  *
      *******************************************************************
       method-id. "GuardarPrimeiroUltimo".

       working-storage section.
       copy "EdObj01.cpy".

       linkage section.
       01 aControl                  object reference.
       01 aPrimeiroK                object reference.
       01 aUltimoK                  object reference.
       01 Flag-Mais-de-50K          pic 9(01) value 0.
          88 Mais-de-50K                      value 1 false 0.

       procedure division using aControl aPrimeiroK aUltimoK
                                Flag-Mais-de-50K.

           move   Flag-Mais-de-50K to Flag-Mais-de-50(Num-Entre-Listas)
           set    LV-Objeto        to aControl
           invoke self "ListaCorrente" using Objetos

           set    Lista-Carregada(Num-Entre-Listas) to true
MARCOS*    set    aPrimeiro      (Num-Entre-Listas) to aPrimeiroK


           if aPrimeiroK not = null
MARCOS         SET aPrimeiro  (Num-Entre-Listas) TO NULL
MARCOS*       invoke aPrimeiro  (Num-Entre-Listas) "finalize"
MARCOS*                            returning aPrimeiro(Num-Entre-Listas)
              invoke aPrimeiroK "concat" using aPrimeiroK returning
                                             aPrimeiro(Num-Entre-Listas)
           end-if

MARCOS     SET aULTIMO  (Num-Entre-Listas) TO NULL
MARCOS*    invoke aUltimo        (Num-Entre-Listas) "finalize"
MARCOS*                            returning aUltimo  (Num-Entre-Listas)
           invoke aUltimoK "concat" using    aUltimoK returning
                                             aUltimo  (Num-Entre-Listas)

      *    set    aUltimo        (Num-Entre-Listas) to aUltimoK

       end method "GuardarPrimeiroUltimo".
      *-----------------------------------------------------------------

       method-id. "PegarPrimeiroLista".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           perform varying Indice from 1 by 1 until Indice > 50 *>or
      *                    Indice > Aux-Num-Itens(Num-Entre-Listas)

               if LV-Num-Itens not = zeros
                  invoke aPrimeiro (Num-Entre-Listas) "at"  using Indice
                                                        returning aTexto
                  subtract 1 from Indice-2
               else
                  invoke aUltimo   (Num-Entre-Listas) "at"  using Indice
                                                        returning aTexto
               end-if

               invoke aTexto "getValue" returning Item

               if Indice = 1
                  move Item(  1: 50 ) to LV-Texto( 1)
                  move Item( 51:100 ) to LV-Texto( 2)
                  move Item(101:150 ) to LV-Texto( 3)
                  move Item(151:200 ) to LV-Texto( 4)
                  move Item(201:250 ) to LV-Texto( 5)
                  move Item(251:300 ) to LV-Texto( 6)
                  move Item(301:350 ) to LV-Texto( 7)
                  move Item(351:400 ) to LV-Texto( 8)
                  move Item(401:450 ) to LV-Texto( 9)
                  move Item(451:500 ) to LV-Texto(10)
                  move Item(501:550 ) to LV-Texto(11)
                  move Item(551:600 ) to LV-Texto(12)
                  move Item(601:650 ) to LV-Texto(13)
                  move Item(651:700 ) to LV-Texto(14)
                  move Item(701:750 ) to LV-Texto(15)
                  move Item(751:800 ) to LV-Texto(16)
                  move Item(801:850 ) to LV-Texto(17)
                  move Item(851:900 ) to LV-Texto(18)
                  move Item(901:950 ) to LV-Texto(19)
                  move Item(951:1000) to LV-Texto(20)
               end-if

               move Item(  1: 50 ) to LV-Coluna-1 (Indice)
               move Item( 51:100 ) to LV-Coluna-2 (Indice)
               move Item(101:150 ) to LV-Coluna-3 (Indice)
               move Item(151:200 ) to LV-Coluna-4 (Indice)
               move Item(201:250 ) to LV-Coluna-5 (Indice)
               move Item(251:300 ) to LV-Coluna-6 (Indice)
               move Item(301:350 ) to LV-Coluna-7 (Indice)
               move Item(351:400 ) to LV-Coluna-8 (Indice)
               move Item(401:450 ) to LV-Coluna-9 (Indice)
               move Item(451:500 ) to LV-Coluna-10(Indice)
               move Item(501:550 ) to LV-Coluna-11(Indice)
               move Item(551:600 ) to LV-Coluna-12(Indice)
               move Item(601:650 ) to LV-Coluna-13(Indice)
               move Item(651:700 ) to LV-Coluna-14(Indice)
               move Item(701:750 ) to LV-Coluna-15(Indice)
               move Item(751:800 ) to LV-Coluna-16(Indice)
               move Item(801:850 ) to LV-Coluna-17(Indice)
               move Item(851:900 ) to LV-Coluna-18(Indice)
               move Item(901:950 ) to LV-Coluna-19(Indice)
               move Item(951:1000) to LV-Coluna-20(Indice)
           end-perform

      *    if Aux-Num-Itens  (Num-Entre-Listas) >= 50 and
      *       Lista-Carregada(Num-Entre-Listas)
      *       move 50 to LV-Num-Itens
      *    end-if

       end method "PegarPrimeiroLista".
      *-----------------------------------------------------------------

       method-id. "PegarUltimoLista".

       working-storage section.
       01 aPriW                   object reference.
       01 aUltW                   object reference.
       01 flag               pic 9(01) value zero.
       01 TrueOrFalse                pic x comp-5.
          88 isTrue                        value 1.
          88 isFalse                       value 0.
       linkage section .
       copy 'EdObj01.cpy'.

       procedure division using objetos  .

           if LV-Num-Itens = zeros
              move zeros        to OO-Contador
           else
              move LV-Num-Itens to OO-Contador
           end-if

           perform varying Indice from 1 by 1 until OO-Contador = 50

               add    1        to OO-Contador
               move   OO-Contador to Indice-2
               invoke aUltimo(Num-Entre-Listas) "at" using Indice-2
                                             returning aTexto
               invoke aTexto      "getValue" returning Item

               invoke aUltimo(Num-Entre-Listas) "includes" using aTexto
                                                  returning TrueOrFalse
               if isFalse
                  exit method
               else
                  move Item(  1: 50 ) to LV-Coluna-1 (Indice)
                                         LV-Texto( 1)
                  move Item( 51:100 ) to LV-Coluna-2 (Indice)
                                         LV-Texto( 2)
                  move Item(101:150 ) to LV-Coluna-3 (Indice)
                                         LV-Texto( 3)
                  move Item(151:200 ) to LV-Coluna-4 (Indice)
                                         LV-Texto( 4)
                  move Item(201:250 ) to LV-Coluna-5 (Indice)
                                         LV-Texto( 5)
                  move Item(251:300 ) to LV-Coluna-6 (Indice)
                                         LV-Texto( 6)
                  move Item(301:350 ) to LV-Coluna-7 (Indice)
                                         LV-Texto( 7)
                  move Item(351:400 ) to LV-Coluna-8 (Indice)
                                         LV-Texto( 8)
                  move Item(401:450 ) to LV-Coluna-9 (Indice)
                                         LV-Texto( 9)
                  move Item(451:500 ) to LV-Coluna-10(Indice)
                                         LV-Texto( 10)
                  move Item(501:550 ) to LV-Coluna-11(Indice)
                                         LV-Texto( 11)
                  move Item(551:600 ) to LV-Coluna-12(Indice)
                                         LV-Texto( 12)
                  move Item(601:650 ) to LV-Coluna-13(Indice)
                                         LV-Texto( 13)
                  move Item(651:700 ) to LV-Coluna-14(Indice)
                                         LV-Texto( 14)
                  move Item(701:750 ) to LV-Coluna-15(Indice)
                                         LV-Texto( 15)
                  move Item(751:800 ) to LV-Coluna-16(Indice)
                                         LV-Texto( 16)
                  move Item(801:850 ) to LV-Coluna-17(Indice)
                                         LV-Texto( 17)
                  move Item(851:900 ) to LV-Coluna-18(Indice)
                                         LV-Texto( 18)
                  move Item(901:950 ) to LV-Coluna-19(Indice)
                                         LV-Texto( 19)
                  move Item(951:1000) to LV-Coluna-20(Indice)
                                         LV-Texto( 20)
               end-if
           end-perform

       end method "PegarUltimoLista".
      *-----------------------------------------------------------------

      *******************************************************************
      *                            INTERNOS                             *
      *******************************************************************
      /method-id. "ApagarListas".

      /linkage section.
      /copy "EdObj01.cpy".

      /procedure division using Objetos.

      **** Limpa as referências e os dados das listas desse programa.
      /    set Fim-Laco to false

      /    perform varying Indice from 1 by 1 until Indice > 100

      /        if OO-Programa-Lista = Programa-Lista(Indice)
      /           initialize          Programa-Lista(Indice)
      /           set aLista            (Indice) to null
      /           set Lista-Carregada   (Indice) to false
      /           set Lista-Inicializada(Indice) to false
      /           set Lista-Secundaria  (Indice) to false
      /        end-if

      /    end-perform

      /end method "ApagarListas".
      *-----------------------------------------------------------------

       method-id. "AtualizarValLista".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           set  Fim-Laco to false

           move 1        to Lista-Corrente

           perform varying Indice from 1 by 1 until Indice > 100 or
                                                           Fim-Laco

               if (   OO-Programa-Lista = Programa-Lista(Indice))
MARCOS*           if (LV-Lista-Corrente = Lista-Corrente        )
MARCOS            if ( lv-objeto = aLista(indice) )

                     set  Fim-Laco     to true
                     set  LV-Objeto    to aLista(Indice)
                     set  Lista-Atual  to aLista(Indice)
                     move Aux-Num-Colunas(Indice) to LV-Num-Colunas
                     move Indice       to Num-Entre-Listas
                     move Endereco-Copy(Num-Entre-Listas)
                       to OO-Endereco-Variavel
                  else
                     add  1            to Lista-Corrente
                  end-if
               end-if
           end-perform

       end method "AtualizarValLista".
      *-----------------------------------------------------------------

       method-id. "PassarReferencia".

       linkage section.
       01 aReferencia            object reference.
       01 Barra-Vertical.
          02 Menor-Cima            pic s9(09) comp-5.
          02 Maior-Cima            pic s9(09) comp-5.
          02 Menor-Baixo           pic s9(09) comp-5.
          02 Maior-Baixo           pic s9(09) comp-5.

       procedure division using aReferencia Barra-Vertical.

           set Fim-Laco to false
           perform varying Indice from 1 by 1 until Indice > 100 or
                                                           Fim-Laco
               if aLista     (Indice) =  null
                  set  Fim-Laco       to true
                  set  aLista(Indice) to aReferencia
                  set  Lista-Atual    to aReferencia
                  set  Lista-Anterior to Lista-Atual
                  move Menor-Cima     to Aux-Menor-Cima (Indice)
                  move Maior-Cima     to Aux-Maior-Cima (Indice)
                  move Menor-Baixo    to Aux-Menor-Baixo(Indice)
                  move Maior-Baixo    to Aux-Maior-Baixo(Indice)
               end-if
           end-perform

       end method "PassarReferencia".
      *-----------------------------------------------------------------

      *******************************************************************
      *                  MÉTODOS GERAIS DO SISTEMA.                     *
      *******************************************************************

      *paragrafo original
       method-id. "CentralizarTelaAMPLA".

       working-storage section.
       01 Altura                     pic 9(09) comp-5.
       01 Subtrair-Altura            pic 9(09) comp-5.
       01 aEventManager              object reference.
       01 aTeste                     object reference.

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.
           move   OO-Handle-Objeto     to    Handle-Objeto

           invoke Window  "fromHandle" using Handle-Objeto
                                       returning   aObjeto

      *--> Get and initialize an EventManager
      *    invoke EventManager   "New" returning aEventManager
      *    invoke aEventManager "Initialize"
      *    invoke aEventManager "GetDesktop" returning aDesktop

      *    invoke aObjeto "SetAncestor" using aMenu
      *    invoke aObjeto "showModal"

      *    invoke aObjeto "getDependents" returning aDesktop
      *    invoke aObjeto "changed" using aDesktop
      *    invoke aObjeto "addDependent" using aDesktop
      *    invoke aObjeto "removeDependencies"
      *    invoke aObjeto "setAncestor" using aDesktop
      *    invoke aObjeto "showModal"
      *---

           invoke aObjeto "GetNativeWidthHeight" using Largura-Objeto
                                                       Altura-Objeto

           invoke SysInformation "GetTitleBarHeight" returning Altura

           move   Altura to Subtrair-Altura
           divide Altura by 2 giving Altura
           add    Altura to Subtrair-Altura

           invoke SysInformation "GetMenuBarHeight" returning Altura

      *    if A barra Iniciar não está oculta
              divide    Altura by 2 giving Altura
      *    end-if

           add Altura to Subtrair-Altura

           subtract Largura-Objeto from OO-Largura-Tela
                                 giving Largura-Objeto
           subtract Altura-Objeto  from OO-Altura-Tela
                                 giving Altura-Objeto

           divide Largura-Objeto by 2 giving Largura-Objeto
           divide Altura-Objeto  by 2 giving Altura-Objeto

           subtract Subtrair-Altura from Altura-Objeto
           invoke aObjeto "SetNativeXY" using Largura-Objeto
                                        Altura-Objeto


       end method "CentralizarTelaAMPLA".
      *-----------------------------------------------------------------

MARCOS*Marcos SOARES
       METHOD-ID. "CentralizarTela-DESATIVADO-23-06-09".

       WORKING-STORAGE SECTION.
        01 Altura                     PIC 9(09)  COMP-5.
        01 Subtrair-Altura            PIC 9(09)  COMP-5.
        01 Altura-Tela                PIC S9(09) COMP-5.
        01 Largura-Tela               PIC S9(09) COMP-5.
       01 aEventManager              object reference.

       LINKAGE SECTION.
       COPY "EdObj01.cpy".

       PROCEDURE DIVISION USING Objetos.
      *---Se a configuração estiver em 800x600 então não faz nada
           if oo-largura-tela <= 800 or oo-altura-tela <= 600
               exit method
           end-if
      *-----------------------


           move   oo-handle-objeto     to    handle-objeto

           invoke window  "fromhandle" using handle-objeto
                                       returning   aobjeto

           move oo-largura-tela    to largura-tela
           move oo-altura-tela     to altura-tela

           invoke aobjeto "getnativewidthheight" using largura-objeto
                                                       altura-objeto

           subtract largura-objeto from largura-tela
           subtract altura-objeto  from altura-tela

           invoke sysinformation "gettitlebarheight" returning altura

           subtract altura from altura-tela

           invoke sysinformation "getmenubarheight" returning altura

           subtract altura from altura-tela

           divide altura-tela   by 2 giving altura-tela
           divide largura-tela  by 2 giving largura-tela

           set aWindow to aObjeto
      *-----busca o pai da Tela
           INVOKE aWindow "GetAncestor" RETURNING aWindow
           INVOKE awindow "GetNativeWidthHeight" USING Largura-Objeto
                                                       Altura-Objeto
      *   Se a tela é menor que 800x600 então não é filho da principal
           if largura-objeto < 800 or altura-objeto < 600
               if oo-numero-1 = 1
                   move 0  to largura-tela altura-tela
               else
                   exit method
               end-if
           end-if
      *------------------------------------------

           INVOKE aObjeto "SetNativeXY" USING Largura-Tela
                                              Altura-Tela
       END METHOD "CentralizarTela-DESATIVADO-23-06-09".
      *-----------------------------------------------------------------

       METHOD-ID. "GetObjeto".

       LINKAGE SECTION.
       COPY "EdObj01.cpy".
       01 objeto object reference.

       PROCEDURE DIVISION USING Objetos RETURNING Objeto.

           move   oo-handle-objeto     to handle-objeto

           invoke window  "fromhandle" using handle-objeto
                                       returning Objeto

       END METHOD "GetObjeto".

      *-----------------------------------------------------------------

       METHOD-ID. "GetHandle".
       LINKAGE SECTION.
           01 OO-HANDLE              OBJECT REFERENCE.

       PROCEDURE DIVISION RETURNING OO-HANDLE.

           invoke window  "fromhandle" using handle-objeto
                                       returning OO-HANDLE

       END METHOD "GetHandle".
      *-----------------------------------------------------------------

soares*
      *Neste novo metodo ele sempre centraliza a tela de acordo com seu
      *pai, resolvendo o preblema de centralizar tela sobre tela que
      *existia na versão anterior
       METHOD-ID. "CentralizarTela".

       WORKING-STORAGE SECTION.
        01 Altura                     PIC 9(09)  COMP-5.
        01 Subtrair-Altura            PIC 9(09)  COMP-5.
        01 Altura-Tela                PIC S9(09) COMP-5.
        01 Largura-Tela               PIC S9(09) COMP-5.
       01 aEventManager              object reference.

       LINKAGE SECTION.
       COPY "EdObj01.cpy".

       PROCEDURE DIVISION USING Objetos.

           move   oo-handle-objeto     to    handle-objeto

           invoke window  "fromhandle" using handle-objeto
                                       returning   aobjeto

           call "BordersOff" using aobjeto

      *-----busca o pai da Tela
           set aWindow to aObjeto
           INVOKE aWindow "GetAncestor" RETURNING aWindow
           INVOKE awindow "GetNativeWidthHeight" USING Largura-Objeto
                                                       Altura-Objeto

           move Largura-Objeto         to Largura-tela
           move Altura-Objeto          to Altura-tela
      *--------------------------------------------------------------


           invoke aobjeto "getnativewidthheight" using largura-objeto
                                                       altura-objeto


           if largura-objeto > largura-tela
           or altura-objeto > altura-tela
               move 1  to largura-tela
                          altura-tela

               invoke aObjeto "SetNativeXY" using Largura-Tela
                                               Altura-Tela
               exit method
           end-if


           subtract largura-objeto from largura-tela
           subtract altura-objeto  from altura-tela

           invoke sysinformation "gettitlebarheight" returning altura

           subtract altura from altura-tela
      *-------
      * altera o ponto de partida
      *    add 30  to altura-tela
      *    add 120 to largura-tela
      *-------------------------------

           invoke sysinformation "getmenubarheight" returning altura

           subtract altura from altura-tela

           divide altura-tela   by 2 giving altura-tela
           divide largura-tela  by 2 giving largura-tela

           invoke aObjeto "SetNativeXY" using Largura-Tela
                                              Altura-Tela
       END METHOD "CentralizarTela".

      *-----------------------------------------------------------------
      *Centraliza a tela apenas na horizontal mantendo o topo estipulado
      *anteriormente.  Hitalo Santos 31/05/2017
       METHOD-ID. "CentralizarTelaHoriz".

       WORKING-STORAGE SECTION.
        01 Altura                     PIC 9(09)  COMP-5.
        01 Altura-Tela                PIC S9(09) COMP-5.
        01 Largura-Tela               PIC S9(09) COMP-5.
        01 Y-Orig-Obj                 pic s9(09) comp-5.
        01 X-Orig-Obj                 pic s9(09) comp-5.
        01 aEventManager              object reference.

       LINKAGE SECTION.
       COPY "EdObj01.cpy".

       PROCEDURE DIVISION USING Objetos.

           move   oo-handle-objeto     to    handle-objeto

           invoke window  "fromhandle" using handle-objeto
                                       returning   aobjeto
           call "BordersOff" using aobjeto
      *-----busca o pai da Tela
           set aWindow to aObjeto
           INVOKE aWindow "GetAncestor" RETURNING aWindow
           INVOKE awindow "GetNativeWidthHeight" USING Largura-Objeto
                                                       Altura-Objeto

           move Largura-Objeto         to Largura-tela
      *    move Altura-Objeto          to Altura-tela
      *-----------------------------------------------------------------


           invoke aobjeto "getnativewidthheight" using largura-objeto
                                                       altura-objeto

           invoke aObjeto "GetNativeXY" using X-Orig-Obj Y-Orig-Obj

           subtract largura-objeto from largura-tela
      *    subtract altura-objeto  from altura-tela

           invoke sysinformation "gettitlebarheight" returning altura

      *    subtract altura from altura-tela
      *-------
      * altera o ponto de partida
      *    add 30  to altura-tela
      *    add 120 to largura-tela
      *-------------------------------

           invoke sysinformation "getmenubarheight" returning altura

      *    subtract altura from altura-tela

      *    divide altura-tela   by 2 giving altura-tela
           divide largura-tela  by 2 giving largura-tela

           invoke aObjeto "SetNativeXY" using Largura-Tela Y-Orig-Obj
      *                                       Altura-Tela
       END METHOD "CentralizarTelaHoriz".

      *-----------------------------------------------------------------
      *Ancora a WIN a margem da direita da Tela
      *Hitalo Santos 31/05/2017
       METHOD-ID. "AncorarTelaDireita".

       WORKING-STORAGE SECTION.
        01 Altura                     PIC 9(09)  COMP-5.
        01 Altura-Tela                PIC S9(09) COMP-5.
        01 Largura-Tela               PIC S9(09) COMP-5.
        01 Y-Orig-Obj                 pic s9(09) comp-5.
        01 X-Orig-Obj                 pic s9(09) comp-5.
        01 aEventManager              object reference.

       LINKAGE SECTION.
       COPY "EdObj01.cpy".
       01 margem-dir                  pic S9(03) comp-5.

       PROCEDURE DIVISION USING Objetos margem-dir.

           move   oo-handle-objeto     to    handle-objeto

           invoke window  "fromhandle" using handle-objeto
                                       returning   aobjeto
           call "BordersOff" using aobjeto
      *-----busca o pai da Tela
           set aWindow to aObjeto
           INVOKE aWindow "GetAncestor" RETURNING aWindow
           INVOKE awindow "GetNativeWidthHeight" USING Largura-Objeto
                                                       Altura-Objeto

           move Largura-Objeto         to Largura-tela
           move Altura-Objeto          to Altura-tela
      *-----------------------------------------------------------------

           invoke aobjeto "getnativewidthheight" using largura-objeto
                                                       altura-objeto

           invoke aObjeto "GetNativeXY" using X-Orig-Obj Y-Orig-Obj

           subtract largura-objeto from largura-tela

           if margem-dir not = 0    *>Margem a direita do objeto
              subtract margem-dir from largura-tela
           else
              subtract 20 from largura-tela
           end-if

           invoke aObjeto "SetNativeXY" using largura-Tela Y-Orig-Obj
           move 0    to margem-dir
       END METHOD "AncorarTelaDireita".

      *-----------------------------------------------------------------
      *Ancora a WIN a margem de baixo da Tela
      *Hitalo Santos 31/05/2017
       METHOD-ID. "AncorarTelaBaixo".

       WORKING-STORAGE SECTION.
        01 Altura                     PIC 9(09)  COMP-5.
        01 Altura-Tela                PIC S9(09) COMP-5.
        01 Largura-Tela               PIC S9(09) COMP-5.
        01 Y-Orig-Obj                 pic s9(09) comp-5.
        01 X-Orig-Obj                 pic s9(09) comp-5.
        01 aEventManager              object reference.

       LINKAGE SECTION.
       COPY "EdObj01.cpy".

       PROCEDURE DIVISION USING Objetos.

           move   oo-handle-objeto     to    handle-objeto

           invoke window  "fromhandle" using handle-objeto
                                       returning   aobjeto
           call "BordersOff" using aobjeto
      *-----busca o pai da Tela
           set aWindow to aObjeto
           INVOKE aWindow "GetAncestor" RETURNING aWindow
           INVOKE awindow "GetNativeWidthHeight" USING Largura-Objeto
                                                       Altura-Objeto

           move Largura-Objeto         to Largura-tela
           move Altura-Objeto          to Altura-tela
      *-----------------------------------------------------------------

           invoke aobjeto "getnativewidthheight" using largura-objeto
                                                       altura-objeto

           invoke aObjeto "GetNativeXY" using X-Orig-Obj Y-Orig-Obj

           subtract largura-objeto from largura-tela
           subtract altura-objeto  from altura-tela

           invoke sysinformation "gettitlebarheight" returning altura

           subtract altura from altura-tela

           invoke sysinformation "getmenubarheight" returning altura

           subtract altura from altura-tela

           *>Margem debaixo do objeto
           subtract 20 from altura-tela

           invoke aObjeto "SetNativeXY" using X-Orig-Obj altura-tela
       END METHOD "AncorarTelaBaixo".

      *-----------------------------------------------------------------
      *Ancora a WIN a margem da direita da Tela
      *Hitalo Santos 12/03/2018
       METHOD-ID. "TirarBordasEf".

       WORKING-STORAGE SECTION.
        01 Altura                     PIC 9(09)  COMP-5.
        01 Altura-Tela                PIC S9(09) COMP-5.
        01 Largura-Tela               PIC S9(09) COMP-5.
        01 Y-Orig-Obj                 pic s9(09) comp-5.
        01 X-Orig-Obj                 pic s9(09) comp-5.
        01 aEventManager              object reference.

       LINKAGE SECTION.
       COPY "EdObj01.cpy".
       01 margem-dir                  pic S9(03) comp-5.

       PROCEDURE DIVISION USING Objetos margem-dir.
           move   oo-handle-objeto     to    handle-objeto
           invoke window  "fromhandle" using handle-objeto
                                       returning   aobjeto
           call "BordersOff" using aobjeto
       END METHOD "TirarBordasEf".

      *-----------------------------------------------------------------
       method-id. "Configuracao".

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.
      **** Limpa as referências e os dados das listas do programa
      *    corrente.
      /    set Fim-Laco to false

           perform varying Indice from 1 by 1 until Indice > 100

               if OO-Programa-Lista = Programa-Lista(Indice)
                  set aLista(Indice) to null
                  initialize Programa-Lista(Indice) Flags-Lista(Indice)
               end-if

           end-perform

      **** Configuracao da tela.
           initialize OO-Config-Value TP-Grupo-Tooltip
           move 15 to OO-Config-Flag
           move 1  to OO-Config-Value
           call "dsrtcfg" using OO-Config-Flag OO-Config-Value

      **** Altura da tela.
           call cc74 "GetSystemMetrics" using by value 1 size 4         1 - 640  x 480
                                               returning OO-Altura-Tela 2 - 800  x 600
      **** Largura da tela.                                             3 - 1024 x 768
           call cc74 "GetSystemMetrics" using by value 0 size 4         se nao for essa mensagem
                                               returning OO-Largura-Tela

          if OO-Programa-Lista = "MENUCTB"
              move   OO-Handle-Objeto    to    Handle-Objeto
              invoke Window "fromHandle" using Handle-Objeto
                                         returning aMenu
           end-if

      /    if Altura-Tela = 480 and Largura-Tela = 640
      /       move 1 to OO-Config-Value
      /       invoke self "CriarMensagem"
      /    end-if

      /    if Altura-Tela = 600 and Largura-Tela = 800
      /       move 2 to OO-Config-Value
      /    end-if

      /    if Altura-Tela = 768 and Largura-Tela = 1024
      /       move 3 to OO-Config-Value
      /    end-if

      /    if OO-Config-Value = zeros
      /       invoke self "CriarMensagem"
      /    end-if

      /    move 9  to OO-Config-Flag
      *    move 2  to OO-Config-Value
      /    call "dsrtcfg" using OO-Config-Flag OO-Config-Value

       end method "Configuracao".
      *-----------------------------------------------------------------
      *alterado em 18-03-2009 por MARCOS SOARES
       method-id. "CriarMensagem".

       working-storage section.
       01 aMbox                  object reference.
       01 aEventManager          object reference.

       linkage section.
           01 texto-mbox         object reference.
           01 titulo-mbox        object reference.
           01 tipo-Mbox          pic x value space.
           01 Cod-Retorno        pic s9(9) comp-5.
      * tipo-Mbox:     ' ' = ok
      *                '1' = yes/no
      *                '2' = ok/cancel

       procedure division using texto-mbox
                                titulo-mbox
                                tipo-mbox
                                cod-retorno.

           invoke EventManager  "New" returning aEventManager
           invoke aEventManager "Initialize"
           invoke aEventManager "GetDesktop" returning aDesktop

           invoke messagebox "new" using aDesktop returning aMbox

           invoke aMbox "create"

           evaluate tipo-Mbox
               when space
                   invoke aMbox "OK"
               when '1'
                   invoke aMbox "YesNo"
               when '2'
                   invoke aMbox "OkCancel"
               when other
                   invoke aMbox "Ok"
           end-evaluate

           invoke aMbox "SetTypeCritical"
           invoke aMbox "setTitle" using titulo-mbox
           invoke aMbox "setMessage" using texto-mbox
           invoke aMbox "show" returning cod-Retorno

       end method "CriarMensagem".
      *-----------------------------------------------------------------

       method-id. "ExibeErro".
           01 texto-mbox         object reference.
           01 titulo-mbox        object reference.
           01 tipo-Mbox          pic x value space.
           01 Cod-Retorno        pic s9(9) comp-5.
           01 lnksize            pic s9(9) comp-5.

       linkage section.
           01  msg-erro    pic x(256).

       procedure division using msg-erro.

           move 256  to lnksize
           invoke Strings "withLengthValue"
               using lnksize msg-erro returning aTexto

           move 10  to lnksize
           invoke Strings "withLengthValue"
               using lnksize, "ATENÇÃO!" returning aTitulo

           move 1 to tipo-mbox
           move 0 to cod-retorno

           invoke self "CriarMensagem" using texto-mbox
                                             titulo-mbox
                                             tipo-mbox
                                             cod-Retorno.

       end method "ExibeErro".
      *-----------------------------------------------------------------
       method-id. "lengthString".
      *Criado por: Rodrigo Dantas 24/12/2013
      *            Welison Geraldo
       linkage section.

       01 ws-campo    pic x(1500).
       01 ws-ind      pic 9999.

       procedure division using ws-campo returning ws-ind.

         move 0                            to ws-ind
         move function reverse(ws-campo)   to ws-campo
         inspect ws-campo tallying ws-ind for leading space
         subtract ws-ind from length of ws-campo giving ws-ind.
         move function reverse(ws-campo)   to ws-campo

         if ws-ind = 0
           move 1                  to ws-ind
         end-if.

       end method "lengthString".
      *-----------------------------------------------------------------
       method-id. "executaComando".
      *Criado por: Saulo Vieira 01/09/2017
       working-storage section.

       77 command-line-len pic x(4) comp-5 value 250.
       77 run-unit-id      pic x(8) comp-5 value 001.
       77 stack-size       pic x(4) comp-5 value 0.
       77 flags            pic x(4) comp-5 value 01.
       77 tty-cmd          pic x(10).
       77 tty-cmd-len      pic x(4) comp-5 value 0.

       linkage section.

       01 ws-comando       pic x(250).
       01 retorno-codigo   pic 9(04) comp-5.

       procedure division using ws-comando returning retorno-codigo.
           call "CBL_EXEC_RUN_UNIT" using ws-comando
                                    by value     command-line-len
                                    by reference run-unit-id
                                    by value     stack-size
                                                 flags
                                    by reference tty-cmd
                                    by value     tty-cmd-len
                                    returning    retorno-codigo


       end method "executaComando".
      *-----------------------------------------------------------------

       method-id. "MontarTolltips".
       working-storage section.
       01 Ind                     pic 9(05).
       01 Ind-B                   pic 9(05).
       01 Texto                   pic x(256).
       01 aTooltip                object reference value null.
       01 Altura-Teste            pic 9(9) comp-5.

       linkage section.
       copy "EdObj01.cpy".

       procedure division using Objetos.

           perform varying Indice from 1 by 1 until Indice > 200 or
                                        TP-Handle-Objeto(Indice) = zeros

               move TP-Handle-Objeto(Indice) to Handle-Objeto

               invoke Window "fromHandle" using Handle-Objeto
                                          returning   aObjeto

               invoke  Tooltip "New" using aObjeto returning aTooltip
               invoke aTooltip "Create"

               move           TP-Texto(Indice) to Texto
               move length of TP-Texto(Indice) to Ind-B

               perform varying Ind from Ind-B by -1 until Ind = 0

                   if Texto(Ind:1) not = spaces and not = x"00"
                      add  1     to Ind
                      move x"00" to Texto(Ind:1)
                      exit perform
                   end-if

               end-perform

               if Ind = 0
                  move x"00" to Texto(Ind-B:1)
               end-if

               invoke Strings "withValue" using    Texto
                                             returning   aTexto

               move 500 to Altura-Teste
               invoke aToolTip "SetHeight" using Altura-Teste

               invoke aToolTip "addGadget" using aObjeto aTexto

               invoke aTexto   "finalize"  returning     aTexto

           end-perform

       end method "MontarTolltips".
      *-----------------------------------------------------------------

      *MARCOS SOARES
      *-Este paragrafo funciona com o ED-LIST
      *-buscando apenas na Lista
       method-id. "Pesquisar".
       working-storage section.
           77 encontrado           pic 99.
           77 lenstring            pic x(004)    comp-5.
           77 byte                 pic 9(05).
           77 i                    pic 9(05).
           77 tam                  pic s9(9)   comp-5.
           77 linhalista           pic x(1200).
           01 Texto                pic x(50).

           77 j                    pic 9(07).
           77 l                    pic 9(02) occurs 20 times.
           01 ws-num-itens         pic 9(9)    comp-5.
           01 lnksize              pic s9(9)   comp-5.

       linkage section.
           copy ed-list.cpb.

           01 List-View           object reference.
           01 texto-pesq          pic x(120).

       procedure division using data-block-lv, list-view, texto-pesq.

           initialize Encontrado I
           invoke List-View "Size" returning Ws-Num-Itens

           if Pesq = space or Texto-Pesq <> Pesq-ant
               move 0  to Position-Y
           end-if

           move texto-pesq to Pesq-ant

           if Position-Y not numeric
               move 0  to Position-Y
           end-if

           move Position-Y  to i

           if i >= Ws-Num-Itens
               move 0  to i
           end-if

           move Texto-Pesq to Pesq

           if Pesq = space
               exit method
           end-if

           perform varying Byte from 117 by -1
                  until Pesq(Byte:1) not equal spaces
           end-perform

           move space to Linhalista

           perform until exit
               add 1 to I
               if i > Ws-Num-Itens
                   exit perform
               end-if

               set Object-Reference to List-View
               move i               to Numeric-Value
               move "retrieve-list-item" to Call-Function
               call "ed-list" using Data-Block-Lv

      *-pegando o tamanho preenchido de cada campo
               perform varying j from 1 by 1 until j = 21
                   perform varying l(j) from 60 by -1 until l(j) = 1 or
                       Lvitem-Text(j)(l(j):1) not equal spaces
                   end-perform
               end-perform
      *--------------------------------------------
               initialize Linhalista

               string Lvitem-Text(1)(1:l(1)) ' '
                      Lvitem-Text(2)(1:l(2)) ' '
                      Lvitem-Text(3)(1:l(3)) ' '
                      Lvitem-Text(4)(1:l(4)) ' '
                      Lvitem-Text(5)(1:l(5)) ' '
                      Lvitem-Text(6)(1:l(6)) ' '
                      Lvitem-Text(7)(1:l(7)) ' '
                      Lvitem-Text(8)(1:l(8)) ' '
                      Lvitem-Text(9)(1:l(9)) ' '
                      Lvitem-Text(10)(1:l(10))
                      Lvitem-Text(11)(1:l(11))
                      Lvitem-Text(12)(1:l(12))
                      Lvitem-Text(13)(1:l(13))
                      Lvitem-Text(14)(1:l(14))
                      Lvitem-Text(15)(1:l(15))
                      Lvitem-Text(16)(1:l(16))
                      Lvitem-Text(17)(1:l(17))
                      Lvitem-Text(18)(1:l(18))
                      Lvitem-Text(19)(1:l(19))
                      Lvitem-Text(20)(1:l(20))
                      into Linhalista

               move 1199 to Lenstring
               call "cbl_toupper" using Linhalista(1:1199)
                                  by value Lenstring

               move Byte to Lenstring
               call "cbl_toupper" using Pesq(1:byte) by value Lenstring

               inspect Linhalista(1:1199) tallying Encontrado
                                          for all Pesq(1:Byte)

               if Encontrado <> 0
                   set Object-Reference to List-View
                   move i               to Numeric-Value
                   move "seleciona"     to Call-Function
                   call "ed-list" using Data-Block-lv

                   set Object-Reference to List-View
                   move "set-focus"     to Call-function
                   call "ed-list" using Data-Block-Lv

                   exit perform
               end-if
           end-perform.

           if Encontrado <> 0
               continue
           else
      *04/01/2018 - Douglas - Quando a area PESQ vier com o valor 999 na
      *ultima posição ao inves do edobj01 criar a menagem de error, vai
      *mover NAO para texto-pesq(118:3) e a area "ERRO-LER-D" terá que
      *ser alimentada para que o erro seja tratado na tela.
             if pesq(118:3) = "999"
               move "NAO"      to texto-pesq(118:3)
             else
               move 0  to Position-Y I
               move "O texto pesquisado não foi encontrado na lista!"
                   to Texto
               move 50  to LnkSize
               invoke Strings "withLengthValue"
                   using LnkSize Texto returning aTexto


               move "ATENÇÃO!" to Texto
               move 10  to LnkSize
               invoke Strings "withLengthValue"
                   using LnkSize Texto returning aTitulo

               invoke Self "CriarMensagem" using aTexto aTitulo ' '
             end-if
           end-if

           move i   to Position-Y.


       end method "Pesquisar".
      *-----------------------------------------------------------------
       method-id. "ListaArquivosDiretorio".
       linkage section.
       77 wArquivo             pic x(256) value spaces.
       77 w-Arquivo            pic x(256) value spaces.
       77 umaColecao           object reference value null.
       77 wTam                 pic s9(9) comp-5 value zeros.

       procedure division using wArquivo, umaColecao, wTam.

          invoke CharacterArray 'withValue' using wArquivo
                                       returning umaMascara
          invoke Filename 'expandWildCards' using umaMascara
                                       returning umaColecao
          invoke umaColecao 'Size' returning wTam.

       end method "ListaArquivosDiretorio".
      *-----------------------------------------------------------------
       method-id. "NomeUltimoArquivo".
       linkage section.
       77 wArquivo             pic x(256) value spaces.
       77 w-Arquivo            pic x(256) value spaces.

       procedure division using wArquivo, w-Arquivo.

          invoke CharacterArray 'withValue' using wArquivo
                                       returning umaMascara
          invoke Filename 'expandWildCards' using umaMascara
                                       returning umaColecao
          invoke umaColecao 'Size' returning wTam
          perform varying lvLoop from 1 by 1 until lvLoop > wTam
             invoke umaColecao 'at' using lvLoop returning umArquivo
             invoke umArquivo 'filenameWithoutDir' returning umNomeArq
             invoke umArquivo 'path' returning umCaminho
             if umArquivo not = null
                invoke umNomeArq 'getValue' returning w-arquivo
                invoke umArquivo 'getValue' returning w-nomecompleto
                invoke umCaminho 'getValue' returning w-caminho
             end-if
          end-perform.

       end method "NomeUltimoArquivo".
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------

       end class-object.

      ******************************************************************
       object.
       object-storage section.

       end object.

      *-----------------------------------------------------------------

       end class EdObj01.
      *-----------------------------------------------------------------
