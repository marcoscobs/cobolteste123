      *****************************************************************
      * The Visual Style entry fields and list boxes show a purple line.
      * The origin of this line is a shadow, which looks nice in the
      * windows standard layout.
      *
      * this program shows, how to invoke the NoBorder method
      * to switch-off this line
      *****************************************************************

       identification division.
       program-id. BordersOff.

       environment division.
       class-control.
          EntryField    is class "txtentry"
          ListBox       is class "listbox"
          TabbedControl is class "tabbctrl"
          .

       WORKING-STORAGE SECTION.
       01 is-EF        pic 99 comp-5.
       01 is-LB        pic 99 comp-5.
       01 is-TABBED    pic 99 comp-5.

       Local-Storage Section.
       01 Child        object reference.
       01 aTab         object reference.

       Linkage Section.
       01 aWindow  object reference.

       procedure Division using aWindow.
       main.
           invoke aWindow "GetFirstChild" returning Child

           perform until Child = NULL
              invoke Child "isTypeOf" using  EntryField returning is-EF
              invoke Child "isTypeOf" using  ListBox    returning is-LB
              if 1 = is-EF or is-LB
                 invoke Child "NoBorder"
              else
                 invoke Child "isTypeOf" using TabbedControl
                                         returning is-Tabbed
                 if 1 = is-Tabbed
                    Perform a-Tabbed-control
                 end-if
              end-if
              invoke child "getNextWindow" returning Child
           end-perform
           exit program.

       a-Tabbed-control.
           invoke Child "GetFirstChild" returning aTab
           perform until aTab = NULL
              Call "BordersOff" using aTab
              invoke aTab "getNextWindow" returning aTab
           end-perform.
       end program BordersOff.

