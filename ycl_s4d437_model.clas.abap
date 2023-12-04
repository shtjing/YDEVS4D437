
CLASS Ycl_s4d437_model DEFINITION
 PUBLIC
 FINAL
 CREATE PUBLIC .

 PUBLIC SECTION.

   CLASS-METHODS class_constructor .
   CLASS-METHODS get_agency_by_user
     IMPORTING
       !iv_user            TYPE syuname DEFAULT sy-uname
     RETURNING
       VALUE(rv_agencynum) TYPE s_agency .
   CLASS-METHODS get_next_travelid_for_agency
     IMPORTING
       !iv_agencynum      TYPE s_agency
     RETURNING
       VALUE(rv_travelid) TYPE Y_travelid .
   CLASS-METHODS authority_check
     IMPORTING
       !iv_agencynum TYPE s_agency
       !iv_actvt     TYPE activ_auth
     RETURNING
       VALUE(rv_rc)  TYPE sysubrc .
 PRIVATE SECTION.

   CONSTANTS first_travelid TYPE Y_travelid VALUE '12000'.
ENDCLASS.



CLASS YCL_S4D437_MODEL IMPLEMENTATION.


 METHOD authority_check.

* This method simulates an authority check

   IF iv_actvt = '01' OR iv_actvt = '02' OR iv_actvt = '06'.

* use mockup
     rv_rc = 4.
     SELECT SINGLE
       FROM Yds4d437_users
   FIELDS 0
     WHERE uname   = @sy-uname
     AND agencynum = @iv_agencynum
   INTO @rv_rc.

     IF iv_actvt = '06' AND iv_agencynum <= '315'.
       " disallow deletion for all existing Agencies
       rv_rc = 4.
     ENDIF.


   ELSE.

* use real authority check
     AUTHORITY-CHECK OBJECT 'S_AGENCY'
     ID 'AGENCYNUM' FIELD iv_agencynum
     ID 'ACTVT' FIELD iv_actvt.

     rv_rc = sy-subrc.
   ENDIF.

 ENDMETHOD.


 METHOD class_constructor.

   DATA lt_mapping TYPE TABLE OF Yds4d437_users .

   SELECT SINGLE
      FROM Yds4d437_users
    FIELDS @abap_true
    INTO @DATA(lv_exists).

   IF lv_exists <> abap_true.

     SELECT FROM stravelag
       FIELDS mandt,
             'TRAIN-##' AS user,
              agencynum,
              @first_travelid  AS last_travelid
       WHERE agencynum >= 100
       ORDER BY agencynum
       INTO TABLE @lt_mapping
       UP TO 41 ROWS.

     LOOP AT lt_mapping ASSIGNING FIELD-SYMBOL(<ls_mapping>).
       REPLACE '##' IN <ls_mapping>-uname WITH CONV numc2( sy-tabix - 1 ).
     ENDLOOP.


     MODIFY Yds4d437_users FROM TABLE lt_mapping.

   ENDIF.

 ENDMETHOD.


 METHOD get_agency_by_user.

* this method simulates a User/Travelagency assignment

   DATA ls_mapping TYPE Yds4d437_users.

   SELECT SINGLE FROM Yds4d437_users
   FIELDS *
   WHERE uname = @iv_user
     INTO @ls_mapping.

   IF sy-subrc <> 0.
     " User not like TRAIN-##: Use travel agency 055
     ls_mapping = VALUE #( uname = sy-uname agencynum = '055' last_travelid = first_travelid ).
     MODIFY Yds4d437_users FROM ls_mapping.
   ENDIF.

   rv_agencynum = ls_mapping-agencynum.

 ENDMETHOD.


 METHOD get_next_travelid_for_agency.

* This method simulates a Number Range Object

   DATA ls_mapping TYPE Yds4d437_users.

   SELECT SINGLE
     FROM Yds4d437_users
   FIELDS *
   WHERE agencynum = @iv_agencynum
   INTO @ls_mapping.

   IF sy-subrc <> 0.
     ls_mapping = VALUE #( agencynum = iv_agencynum last_travelid = first_travelid ).
   ENDIF.

   ls_mapping-last_travelid = ls_mapping-last_travelid + 1.

   MODIFY Yds4d437_users FROM ls_mapping.

   rv_travelid = ls_mapping-last_travelid.

 ENDMETHOD.
ENDCLASS.
