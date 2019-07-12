      $set mfoo case
      *>-----------------------------------------------------------
      *> Class description
      *>-----------------------------------------------------------
       copy "windows.cpy".
       class-id. owndrawlistview data is protected
                 inherits from listview with data.

       copy "callconv.cpy".

       object section.
       class-control.
           owndrawlistview is class "owndrawlistview"
      *> OCWIZARD - start list of classes
           listview is class "listview"
      *> OCWIZARD - end list of classes
      *>---USER-CODE. Add any additional class names below.
           OrderedCollection is class "ordrdcll"
           color is class "color"
           font is class "font"
           .

      *>-----------------------------------------------------------
       working-storage section. *> Definition of global data
      *>-----------------------------------------------------------
      $IF DAVSDEMO DEFINED
      *01  custom-font-flag        pic 9 is external.
       01  custom-color-flag       pic 9 is external.
      *
      $END
      *>-----------------------------------------------------------
       class-object.   *> Definition of class data and methods
      *>-----------------------------------------------------------
       object-storage section.

      *> OCWIZARD - start standard class methods
      *> OCWIZARD - end standard class methods

       end class-object.

      *>-----------------------------------------------------------
       object.         *> Definition of instance data and methods
      *>-----------------------------------------------------------
       object-storage section.

       01  font-size               PIC 9(9) COMP-5 value 0.

      *> OCWIZARD - start standard instance methods

       method-id. "wmGadgetNotify".
       thread-local-storage section.

       local-storage section.

       01  acolor   object reference.
       01  anlvitem object reference.
       01  colref   colorref.
       01  itemindex   pic s9(9) comp-5.
       01  afont    object reference.
       01  anHFONT  HFONT.
       01  theFont                 usage object reference.
       01  theFontName             usage object reference.
       01  font-styles             PIC 9(9) COMP-5 value 1.


       linkage section.
       01 lnkWnd       HWND.
       01 lnkMsg       INT.
       01 lnkwParam    WPARAM.
       01 lnklParam    POINTER.
       01 lnkNMHDR     NMHDR.
       01 mResult      LRESULT.

       01 lnknm        NMLVCUSTOMDRAW.


       procedure division using lnkWnd lnkMsg lnkwParam lnklParam
           returning mResult.

           set address of lnkNMHDR to lnklParam
           evaluate 1code of lnkNMHDR
               when NM-CUSTOMDRAW
                   set address of lnknm to lnklparam
      *            DISPLAY "NM-CUSTOMDRAW"
                   evaluate dwDrawStage of lnknm
                       when CDDS-PREPAINT
      *                    display "CDDS-PREPAINT"
      *> Set Flag so we get notified when an individual element
      *> needs to be drawn.
                           move CDRF-NOTIFYITEMDRAW to mResult
                       when CDDS-ITEMPREPAINT
      *                    DISPLAY "CDDS-ITEMPREPAINT" dwItemSpec of
      *                                                        lnknm
                           add 1 to dwItemSpec of lnknm  *> COBOL works
                               giving itemindex          *> from 1
      *> We have used a custom listviewitem that hold the color
      *> for the item. We need to retrieve the listviewitem and
      *> get the color from it.
                           invoke self "itematindex" using
                              itemindex returning anlvitem
      $IF DAVSDEMO DEFINED
                          if custom-color-flag = 1
      $END
                               invoke anlvitem "getcolor" returning
                                            clrTextBk of lnknm
      *-------22/04/07
      *                        invoke anlvitem "getcolor" returning
      *                                acolor
      *                        invoke acolor "ascolorref" returning
      *                                clrTextBk of lnknm

      *                    end-if
                           invoke super "wmGadgetNotify" using lnkWnd
                                           lnkMsg lnkwParam lnklParam
                           returning mResult
      *---22/04/07
                   end-evaluate

           when other
               invoke super "wmGadgetNotify" using lnkWnd lnkMsg
                                                   lnkwParam lnklParam
                     returning mResult
       end-evaluate

       exit program
       end method "wmGadgetNotify".

       method-id. "redrawallitems".
       local-storage section.
       01 lsSize pic x(4) comp-5.
       01 lsBool BOOL.
       procedure division.
           if isCreated
                invoke self "size" returning lsSize
                call WAPI SendMessage using by value p2-id
                                  by value LVM-REDRAWITEMS
                                  by value 0
                                  by value lsSize
                        returning lsBool
                call WAPI UpdateWindow using p2-id
                        returning lsBool
           end-if
           exit method.
       end method "redrawallitems".

      *> OCWIZARD - end standard instance methods

       end object.

       end class owndrawlistview.

