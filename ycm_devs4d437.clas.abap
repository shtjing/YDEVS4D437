class YCM_DEVS4D437 definition
 public
 inheriting from cx_Static_check
 final
 create public .

public section.

   interfaces if_abap_behv_message.

 constants:
   BEGIN OF cancel_success,
       msgid TYPE symsgid VALUE 'DEVS4D437',
       msgno TYPE symsgno VALUE '120',
       attr1 TYPE scx_attrname VALUE 'TRAVELID',
       attr2 TYPE scx_attrname VALUE 'STARTDATE_TXT',
       attr3 TYPE scx_attrname VALUE '',
       attr4 TYPE scx_attrname VALUE '',
     END OF cancel_success .
 constants:
   BEGIN OF already_cancelled,
       msgid TYPE symsgid VALUE 'DEVS4D437',
       msgno TYPE symsgno VALUE '130',
       attr1 TYPE scx_attrname VALUE '',
       attr2 TYPE scx_attrname VALUE '',
       attr3 TYPE scx_attrname VALUE '',
       attr4 TYPE scx_attrname VALUE '',
     END OF already_cancelled .
 constants:
   BEGIN OF field_empty,
       msgid TYPE symsgid VALUE 'DEVS4D437',
       msgno TYPE symsgno VALUE '200',
       attr1 TYPE scx_attrname VALUE '',
       attr2 TYPE scx_attrname VALUE '',
       attr3 TYPE scx_attrname VALUE '',
       attr4 TYPE scx_attrname VALUE '',
     END OF field_empty .
 constants:
   BEGIN OF customer_not_exist,
       msgid TYPE symsgid VALUE 'DEVS4D437',
       msgno TYPE symsgno VALUE '210',
       attr1 TYPE scx_attrname VALUE 'CUSTOMERID',
       attr2 TYPE scx_attrname VALUE '',
       attr3 TYPE scx_attrname VALUE '',
       attr4 TYPE scx_attrname VALUE '',
     END OF customer_not_exist .
 constants:
   BEGIN OF dates_wrong_sequence,
       msgid TYPE symsgid VALUE 'DEVS4D437',
       msgno TYPE symsgno VALUE '220',
       attr1 TYPE scx_attrname VALUE '',
       attr2 TYPE scx_attrname VALUE '',
       attr3 TYPE scx_attrname VALUE '',
       attr4 TYPE scx_attrname VALUE '',
     END OF dates_wrong_sequence .
 constants:
   BEGIN OF start_date_past,
       msgid TYPE symsgid VALUE 'DEVS4D437',
       msgno TYPE symsgno VALUE '230',
       attr1 TYPE scx_attrname VALUE '',
       attr2 TYPE scx_attrname VALUE '',
       attr3 TYPE scx_attrname VALUE '',
       attr4 TYPE scx_attrname VALUE '',
     END OF start_date_past .
 constants:
   BEGIN OF end_date_past,
       msgid TYPE symsgid VALUE 'DEVS4D437',
       msgno TYPE symsgno VALUE '240',
       attr1 TYPE scx_attrname VALUE '',
       attr2 TYPE scx_attrname VALUE '',
       attr3 TYPE scx_attrname VALUE '',
       attr4 TYPE scx_attrname VALUE '',
     END OF end_date_past .
 constants:
   BEGIN OF class_invalid,
       msgid TYPE symsgid VALUE 'DEVS4D437',
       msgno TYPE symsgno VALUE '300',
       attr1 TYPE scx_attrname VALUE 'FLIGHTCLASS',
       attr2 TYPE scx_attrname VALUE '',
       attr3 TYPE scx_attrname VALUE '',
       attr4 TYPE scx_attrname VALUE '',
     END OF class_invalid .
 constants:
   BEGIN OF flight_date_past,
       msgid TYPE symsgid VALUE 'DEVS4D437',
       msgno TYPE symsgno VALUE '310',
       attr1 TYPE scx_attrname VALUE ' ',
       attr2 TYPE scx_attrname VALUE '',
       attr3 TYPE scx_attrname VALUE '',
       attr4 TYPE scx_attrname VALUE '',
     END OF flight_date_past .
 constants:
   BEGIN OF flight_not_exist,
       msgid TYPE symsgid VALUE 'DEVS4D437',
       msgno TYPE symsgno VALUE '320',
       attr1 TYPE scx_attrname VALUE 'CARRIERID',
       attr2 TYPE scx_attrname VALUE 'CONNECTIONID',
       attr3 TYPE scx_attrname VALUE 'FLIGHTDATE_TXT',
       attr4 TYPE scx_attrname VALUE '',
     END OF flight_not_exist .
 data TRAVELID type Y_TRAVELID read-only .
 data STARTDATE_TXT type CHAR10 read-only .
 data CUSTOMERID type S_CUSTOMER read-only .
 data FLIGHTCLASS type S_CLASS read-only .
 data CARRIERID type S_CARR_ID read-only .
 data CONNECTIONID type S_CONN_ID read-only .
 data FLIGHTDATE_TXT type CHAR10 read-only .

  ALIASES severity for if_abap_behv_message~severity.

 methods CONSTRUCTOR
   importing
     !TEXTID like IF_T100_MESSAGE=>T100KEY optional
     !PREVIOUS like PREVIOUS optional
     !SEVERITY  type if_abap_behv_message=>t_severity OPTIONAL
     !TRAVELID type Y_TRAVELID optional
     !STARTDATE type Y_STDAT optional
     !CUSTOMERID type S_CUSTOMER optional
     !FLIGHTCLASS type S_CLASS optional
     !CARRIERID type S_CARR_ID optional
     !CONNECTIONID type S_CONN_ID optional
     !FLIGHTDATE type S_DATE optional .
 PROTECTED SECTION.
private section.
ENDCLASS.



CLASS YCM_DEVS4D437 IMPLEMENTATION.


 METHOD constructor ##ADT_SUPPRESS_GENERATION.
   CALL METHOD super->constructor
     EXPORTING
       previous                = previous
        .

   CLEAR me->textid.
   IF textid IS INITIAL.
     if_t100_message~t100key = if_t100_message=>default_textid.
   ELSE.
     if_t100_message~t100key = textid.
   ENDIF.

   me->if_abap_behv_message~m_severity = severity.

   me->travelid = travelid.
*    WRITE startdate TO me->startdate_txt.
   me->startdate_txt = |{ startdate DATE = USER }|.

   me->customerid =  customerid.

   me->flightclass = flightclass.

   me->carrierid = carrierid.
   me->connectionid = connectionid.
   me->flightdate_txt = |{ flightdate date = user }|.

 ENDMETHOD.
ENDCLASS.
