CLASS Ycm_d437e_travel DEFINITION
 PUBLIC
 INHERITING FROM cx_static_check
 FINAL
 CREATE PUBLIC .

 PUBLIC SECTION.

   INTERFACES if_abap_behv_message.
   INTERFACES if_t100_dyn_msg .
   INTERFACES if_t100_message .

   CONSTANTS:
     BEGIN OF cm_d437e_travel,
       msgid TYPE symsgid VALUE 'DEVS4D437',
       msgno TYPE symsgno VALUE '100',
       attr1 TYPE scx_attrname VALUE '',
       attr2 TYPE scx_attrname VALUE '',
       attr3 TYPE scx_attrname VALUE '',
       attr4 TYPE scx_attrname VALUE '',
     END OF cm_d437e_travel .

   CONSTANTS:
     BEGIN OF cancel_success,
       msgid TYPE symsgid VALUE 'DEVS4D437',
       msgno TYPE symsgno VALUE '120',
       attr1 TYPE scx_attrname VALUE '',
       attr2 TYPE scx_attrname VALUE '',
       attr3 TYPE scx_attrname VALUE '',
       attr4 TYPE scx_attrname VALUE '',
     END OF cancel_success.

   CONSTANTS:
     BEGIN OF already_cancelled,
       msgid TYPE symsgid VALUE 'DEVS4D437',
       msgno TYPE symsgno VALUE '130',
       attr1 TYPE scx_attrname VALUE '',
       attr2 TYPE scx_attrname VALUE '',
       attr3 TYPE scx_attrname VALUE '',
       attr4 TYPE scx_attrname VALUE '',
     END OF already_cancelled.

   METHODS constructor
     IMPORTING
       !textid   LIKE if_t100_message=>t100key OPTIONAL
       !previous LIKE previous OPTIONAL
       !severity TYPE if_abap_behv_message~t_severity OPTIONAL
     .

 PROTECTED SECTION.
 PRIVATE SECTION.

ENDCLASS.



CLASS YCM_D437E_TRAVEL IMPLEMENTATION.


 METHOD constructor ##ADT_SUPPRESS_GENERATION.
   CALL METHOD super->constructor
     EXPORTING
       previous = previous.

   if_abap_behv_message~m_severity = severity.

   CLEAR me->textid.
   IF textid IS INITIAL.
     if_t100_message~t100key = if_t100_message=>default_textid.
   ELSE.
     if_t100_message~t100key = textid.
   ENDIF.
 ENDMETHOD.
ENDCLASS.
