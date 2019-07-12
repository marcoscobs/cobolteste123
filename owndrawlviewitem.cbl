      $set mfoo case
      *>-----------------------------------------------------------
      *> Class description
      *>-----------------------------------------------------------
       copy "windows.cpy".
       class-id. owndrawlviewitem data is protected
                 inherits from listviewitem with data.

       copy "callconv.cpy".

       object section.
       class-control.
           owndrawlviewitem is class "owndrawlviewitem"
      *> OCWIZARD - start list of classes
           ListViewItem is class "lstvitem"
      *> OCWIZARD - end list of classes
      *>---USER-CODE. Add any additional class names below.
           OrderedCollection is class "ordrdcll"
           color is class "color"
           .

      *>-----------------------------------------------------------
       working-storage section. *> Definition of global data
      *>-----------------------------------------------------------

      *>-----------------------------------------------------------
       class-object.   *> Definition of class data and methods
      *>-----------------------------------------------------------
       object-storage section.

      *> OCWIZARD - start standard class methods
      *> OCWIZARD - end standard class methods
      *>----------------------------------------------------------------
      *> Creates a new list view item with lnkNumColumns columns.
      *>----------------------------------------------------------------
       method-id. "new".
       linkage section.
       01 lnkNumColumns         pic x(4) comp-5.
       01 lnkListViewItem       usage object reference.

       procedure division using lnkNumColumns
                        returning lnkListViewItem.
           invoke super "new" using lnkNumColumns
               returning lnkLIstViewItem
           exit method.
       end method "new".

       end class-object.

      *>-----------------------------------------------------------
       object.         *> Definition of instance data and methods
      *>-----------------------------------------------------------
       object-storage section.

      *> OCWIZARD - start standard instance methods

       01  colref   colorref   value 0.
       01  itemfont object reference.

       method-id. "setColor".
       thread-local-storage section.

       local-storage section.


       linkage section.
       01 lnkcolref    colorref.

       procedure division using lnkcolref.

           move lnkcolref to colref

       exit program
       end method "setColor".

       method-id. "getColor".
       thread-local-storage section.

       local-storage section.


       linkage section.
       01 lnkcolref    colorref.

       procedure division returning lnkcolref.

      *    if colref = 128
      *       move 255 to colref
      *    end-if
      *
      *    if colref = 255
      *       move 220 to colref
      *    end-if

           move colref to lnkcolref

       exit program
       end method "getColor".

       method-id. "setFont".
       thread-local-storage section.

       local-storage section.


       linkage section.
       01 lnkfont    object reference.

       procedure division using lnkfont.

           invoke lnkfont "deepcopy" returning itemfont

       exit program
       end method "setFont".

       method-id. "getFont".
       thread-local-storage section.

       local-storage section.


       linkage section.
       01 lnkfont    object reference.

       procedure division returning lnkfont.

           move itemfont to lnkfont

       exit program
       end method "getFont".

       method-id. "finalize".
       thread-local-storage section.

       local-storage section.


       linkage section.
       01 lnkobj    object reference.

       procedure division returning lnkobj.

           invoke itemfont "finalize" returning itemfont
           invoke super "finalize" returning itemfont

       exit program
       end method "finalize".




      *> OCWIZARD - end standard instance methods

       end object.

       end class owndrawlviewitem.


