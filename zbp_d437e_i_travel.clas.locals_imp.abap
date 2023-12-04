"抬头
CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.

 PRIVATE SECTION.

   METHODS issue_message FOR MODIFY
     IMPORTING keys FOR ACTION travel~issue_message.
   METHODS set_to_cancelled FOR MODIFY
     IMPORTING keys FOR ACTION travel~set_to_cancelled.
   METHODS get_authorizations FOR AUTHORIZATION
     IMPORTING keys REQUEST requested_authorizations FOR travel RESULT result.
   METHODS validatecustomer FOR VALIDATE ON SAVE
     IMPORTING keys FOR travel~validatecustomer.



   METHODS determinesemantickey FOR DETERMINE ON MODIFY
     IMPORTING keys FOR travel~determinesemantickey.
   METHODS get_features FOR FEATURES
     IMPORTING keys REQUEST requested_features FOR travel RESULT result.
   METHODS validatestartdate FOR VALIDATE ON SAVE
     IMPORTING keys FOR travel~validatestartdate.
   METHODS validateenddate FOR VALIDATE ON SAVE
     IMPORTING keys FOR travel~validateenddate.
   METHODS validatesequence FOR VALIDATE ON SAVE
     IMPORTING keys FOR travel~validatesequence.


ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

 METHOD issue_message.

   DATA lo_msg TYPE REF TO YCM_D437E_TRAVEL.

   CREATE OBJECT lo_msg
     EXPORTING
       textid   = YCM_D437E_TRAVEL=>CM_D437E_TRAVEL
       severity = YCM_D437E_TRAVEL=>if_abap_behv_message~severity-success.

   APPEND lo_msg TO reported-%other.

* Alternative coding with use of expression NEW
*-----------------------------------------------*
*    APPEND NEW YCM_D437E_TRAVEL( textid = YCM_D437E_TRAVEL=>YCM_D437E_TRAVEL
*                                severity = YCM_D437E_TRAVEL=>if_abap_behv_message~severity-success )
*       TO reported-%other.


* Alternative coding with use of expressions NEW and VALUE
*-----------------------------------------------------------*
*    reported-%other =
*            VALUE #( BASE reported-%other
*                     ( NEW YCM_D437E_TRAVEL( textid = YCM_D437E_TRAVEL=>YCM_D437E_TRAVEL
*                                            severity = YCM_D437E_TRAVEL=>if_abap_behv_message~severity-success
*                                          )
*                     )
*                   ).


 ENDMETHOD.

 METHOD set_to_cancelled.

   DATA lo_msg TYPE REF TO Ycm_devs4d437.

   DATA ls_reported_travel LIKE LINE OF reported-travel.

   DATA lt_read_in TYPE TABLE FOR READ IMPORT Yd437e_i_travel.
   DATA ls_read_in LIKE LINE OF lt_read_in.

   DATA lt_travel TYPE TABLE FOR READ RESULT Yd437e_i_travel.

   DATA lt_update TYPE TABLE FOR UPDATE Yd437e_i_travel.
   DATA ls_update LIKE LINE OF lt_update.

   DATA ls_failed TYPE RESPONSE FOR FAILED Yd437e_i_travel.

   FIELD-SYMBOLS <ls_key> LIKE LINE OF keys.
   FIELD-SYMBOLS <ls_travel> LIKE LINE OF lt_travel.

   DATA ls_key LIKE LINE OF keys.

   LOOP AT keys ASSIGNING <ls_key>.
     MOVE-CORRESPONDING <ls_key> TO ls_read_in.
     APPEND ls_read_in TO lt_read_in.
   ENDLOOP.

   READ ENTITY IN LOCAL MODE Yd437e_i_travel
       ALL FIELDS WITH lt_read_in
       RESULT lt_travel.

   LOOP AT lt_travel ASSIGNING <ls_travel>.

     IF <ls_travel>-status = 'C'. "already cancelled
       CREATE OBJECT lo_msg
         EXPORTING
           textid   = YCM_D437E_TRAVEL=>already_cancelled
           severity = if_abap_behv_message=>severity-error.

       ls_reported_travel-%tky = <ls_travel>-%tky.
       ls_reported_travel-%msg = lo_msg.

       APPEND ls_reported_travel TO reported-travel.

     ELSE.
       CLEAR lt_update.
       ls_update-%tky   = <ls_travel>-%tky.
       ls_update-status = 'C'.

       APPEND ls_update TO lt_update.

* Update Status of travel not yet cancelled

       MODIFY ENTITY IN LOCAL MODE Yd437e_i_travel
          UPDATE FIELDS (  status ) WITH lt_update
          FAILED ls_failed.

       IF ls_failed IS INITIAL.
* Success message
         CREATE OBJECT lo_msg
           EXPORTING
             textid   = YCM_D437E_TRAVEL=>cancel_success
             severity = if_abap_behv_message=>severity-success.

         ls_reported_travel-%key = <ls_travel>-%key.
         ls_reported_travel-%msg = lo_msg.

         APPEND ls_reported_travel TO reported-travel.

       ENDIF.
     ENDIF.

   ENDLOOP.


* Alternative Coding using CORRESPONDING, VALUE and Inline Declarations

*    READ ENTITY IN LOCAL MODE d437e_i_travel
*        ALL FIELDS WITH CORRESPONDING #( keys )
*        RESULT DATA(lt_travel).
*
*    LOOP AT lt_travel ASSIGNING FIELD-SYMBOL(<ls_travel>).
*
*      IF <ls_travel>-status = 'C'. "already cancelled
*
*        APPEND VALUE #( %tky = <ls_travel>-%tky
*                        %msg = NEW YCM_D437E_TRAVEL(
*                                  textid   = YCM_D437E_TRAVEL=>already_cancelled
*                                  severity = if_abap_behv_message=>severity-error
*                                                  )
*                      )
*           TO reported-travel.
*
*      ELSE.
*
** Update Status of travel
*
*        MODIFY ENTITY IN LOCAL MODE d437e_i_travel
*           UPDATE FIELDS (  status )
*                    WITH VALUE #(
*                                  ( %tky = <ls_travel>-%tky
*                                    status = 'C'
*                                  )
*                                )
*           FAILED DATA(ls_failed).
*
*        IF ls_failed IS INITIAL.
*
*          APPEND VALUE #( %tky = <ls_travel>-%tky
*                          %msg = NEW YCM_D437E_TRAVEL(
*                                   textid   = YCM_D437E_TRAVEL=>cancel_success
*                                   severity = if_abap_behv_message=>severity-success
*                                                    )
*                        )
*            TO reported-travel.
*
*        ENDIF.
*      ENDIF.
*
*    ENDLOOP.

 ENDMETHOD.

 METHOD get_authorizations.

*    IF    requested_authorizations-%update                   = if_abap_behv=>mk-on
*      OR  requested_authorizations-%action-set_to_cancelled  = if_abap_behv=>mk-on.

   READ ENTITY IN LOCAL MODE Yd437e_i_travel
          FIELDS ( agencyid )  WITH CORRESPONDING #( keys )
            RESULT DATA(lt_travel).

   LOOP AT lt_travel ASSIGNING FIELD-SYMBOL(<ls_travel>).
*        AUTHORITY-CHECK OBJECT 'S_AGENCY'
*            ID 'AGENCYNUM' FIELD <ls_travel>-agencyid
*            ID 'ACTVT'     FIELD '02'.
*
*        IF sy-subrc <> 0.

     "Use simulation of different roles for different users
     DATA(lv_subrc) = Ycl_s4d437_model=>authority_check(
                          EXPORTING
                            iv_agencynum = <ls_travel>-agencyid
                            iv_actvt     = '02'
                        ).

     IF lv_subrc <> 0.

       APPEND VALUE #( %tky = <ls_travel>-%tky
                       %update = if_abap_behv=>auth-unauthorized
                       %action-set_to_cancelled = if_abap_behv=>auth-unauthorized
                     )
            TO result.


     ENDIF.

   ENDLOOP.
*    ENDIF.


 ENDMETHOD.

 METHOD validatecustomer.

* for message object
   DATA lo_msg TYPE REF TO Ycm_devs4d437.

* work areas for response parameters
   DATA ls_reported_travel LIKE LINE OF reported-travel.
   DATA ls_failed_travel   LIKE LINE OF failed-travel.

* Constant for state area (needed for validation messages in draft)

   CONSTANTS c_state TYPE string VALUE `CUSTOMER`.

* read required data
**********************************************************************
   READ ENTITY IN LOCAL MODE Yd437e_i_travel
      FIELDS ( customerid ) WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travel).

   LOOP AT lt_travel ASSIGNING FIELD-SYMBOL(<ls_travel>).

* New for Draft: Add new line to to reported
*                to delete previous messages of same state area
**********************************************************************
     CLEAR ls_reported_travel.
     MOVE-CORRESPONDING  <ls_travel> TO ls_reported_travel.
     ls_reported_travel-%state_area = c_state .
     APPEND ls_reported_travel TO reported-travel.

     "expression-based alternative
*      APPEND VALUE #( %tky        = <ls_travel>-%tky
*                      %state_area = c_state )
*          TO reported-travel.

* validate data and create message object in case of error
**********************************************************************

     IF <ls_travel>-customerid IS INITIAL.
       "error because of initial input field
       CREATE OBJECT lo_msg
         EXPORTING
           textid   = Ycm_devs4d437=>field_empty
           severity = Ycm_devs4d437=>severity-error.

       " expression-based alternative
*        lo_msg = new #( textid   = cm_devs4d437=>field_empty
*                        severity = cm_devs4d437=>severity-error ).
     ELSE.
       "existence check for customer
       SELECT SINGLE @abap_true
                FROM Yd437_i_customer
                INTO @DATA(lv_exists)
               WHERE customer = @<ls_travel>-customerid.

       IF lv_exists <> abap_true.
         " error because of non-existent customer
         CREATE OBJECT lo_msg
           EXPORTING
             textid     = Ycm_devs4d437=>customer_not_exist
             customerid = <ls_travel>-customerid
             severity   = Ycm_devs4d437=>severity-error.

         " expression-based alternative
*           lo_msg = new #(
*                       textid     = cm_devs4d437=>customer_not_exist
*                       customerid = <ls_travel>-customerid
*                       severity   = cm_devs4d437=>severity-error
*                         ).
       ENDIF.

     ENDIF.

* report message and mark flight travel as failed
**********************************************************************
     IF lo_msg IS BOUND.

       CLEAR ls_failed_travel.
       MOVE-CORRESPONDING <ls_travel> TO ls_failed_travel.
       APPEND ls_failed_travel TO failed-travel.

       CLEAR ls_reported_travel.
       MOVE-CORRESPONDING <ls_travel> TO ls_reported_travel.
       ls_reported_travel-%element-customerid = if_abap_behv=>mk-on.
       ls_reported_travel-%msg = lo_msg.
       ls_reported_travel-%state_area = c_state.
       APPEND ls_reported_travel TO reported-travel.

       " expression-based alternative without helper variables
*       APPEND CORRESPONDING #( <ls_travel> )
*           TO failed-travel.
*
*       APPEND VALUE #(
*          %tky     = <ls_travel>-%tky
*          %element = VALUE #( customerid = if_abap_behv=>mk-on )
*          %msg     = NEW cm_devs4d437(
*                       textid     = cm_devs4d437=>customer_not_exist
*                       customerid = <ls_travel>-customerid
*                       severity   = cm_devs4d437=>severity-error
*                                 )
*          %state_area = c_state
*                     )
*            TO reported-travel.
*
       CLEAR lo_msg.
     ENDIF.

   ENDLOOP.

 ENDMETHOD.

 METHOD validatestartdate.

   CONSTANTS c_area TYPE string VALUE `STARTDATE`.

   READ ENTITY IN LOCAL MODE Yd437e_i_travel
        FIELDS ( startdate ) WITH CORRESPONDING #( keys )
        RESULT DATA(lt_travel).

   LOOP AT lt_travel ASSIGNING FIELD-SYMBOL(<ls_travel>).

     APPEND VALUE #( %tky    = <ls_travel>-%tky
                     %state_area = c_area )
         TO reported-travel.

     "Start Date
     "----------"
     IF <ls_travel>-startdate IS INITIAL.

       APPEND CORRESPONDING #( <ls_travel> )
           TO failed-travel.
       APPEND VALUE #(
              %tky     = <ls_travel>-%tky
              %element = VALUE #( startdate = if_abap_behv=>mk-on )
              %msg     = NEW Ycm_devs4d437(
                           textid   = Ycm_devs4d437=>field_empty
                           severity = Ycm_devs4d437=>severity-error
                                        )
               %state_area = c_area
                     )
           TO reported-travel.

     ELSEIF <ls_travel>-startdate < sy-datum.
       " or use cl_abap_context_info=>get_system_date( )

       APPEND CORRESPONDING #( <ls_travel> )
           TO failed-travel.
       APPEND VALUE #(
              %tky     = <ls_travel>-%tky
              %element = VALUE #( startdate = if_abap_behv=>mk-on )
              %msg     = NEW Ycm_devs4d437(
                           textid   = Ycm_devs4d437=>start_date_past
                           severity = Ycm_devs4d437=>severity-error
                                         )
              %state_area = c_area
                    )
           TO reported-travel.

     ENDIF.

   ENDLOOP.
 ENDMETHOD.

 METHOD validateenddate.

   CONSTANTS c_area TYPE string VALUE `ENDDATE`.

   READ ENTITY IN LOCAL MODE Yd437e_i_travel
        FIELDS ( enddate ) WITH CORRESPONDING #( keys )
        RESULT DATA(lt_travel).

   LOOP AT lt_travel ASSIGNING FIELD-SYMBOL(<ls_travel>).

     APPEND VALUE #( %tky    = <ls_travel>-%tky
                      %state_area = c_area )
          TO reported-travel.

     "End Date
     "----------"
     IF <ls_travel>-enddate IS INITIAL.

       APPEND CORRESPONDING #( <ls_travel> )
           TO failed-travel.
       APPEND VALUE #(
              %tky     = <ls_travel>-%tky
              %element = VALUE #( enddate = if_abap_behv=>mk-on )
              %msg     = NEW Ycm_devs4d437(
                             textid   = Ycm_devs4d437=>field_empty
                             severity = Ycm_devs4d437=>severity-error
                                        )
               %state_area = c_area
                      )
           TO reported-travel.

     ELSEIF <ls_travel>-enddate < sy-datum.
       " or use cl_abap_context_info=>get_system_date( )

       APPEND CORRESPONDING #( <ls_travel> )
           TO failed-travel.
       APPEND VALUE #(
              %tky     = <ls_travel>-%tky
              %element = VALUE #( enddate = if_abap_behv=>mk-on )
              %msg     = NEW Ycm_devs4d437(
                             textid   = Ycm_devs4d437=>end_date_past
                             severity = Ycm_devs4d437=>severity-error
                                         )
              %state_area = c_area
                     )
           TO reported-travel.

     ENDIF.

   ENDLOOP.
 ENDMETHOD.

 METHOD validatesequence.

   CONSTANTS c_area TYPE string VALUE `SEQUENCE`.

   READ ENTITY IN LOCAL MODE Yd437e_i_travel
        FIELDS ( startdate enddate ) WITH CORRESPONDING #( keys )
        RESULT DATA(lt_travel).

   LOOP AT lt_travel ASSIGNING FIELD-SYMBOL(<ls_travel>).

     APPEND VALUE #( %tky    = <ls_travel>-%tky
                      %state_area = c_area )
          TO reported-travel.

     "Sequence of Dates
     "-----------------"
     IF <ls_travel>-startdate IS INITIAL
     OR <ls_travel>-enddate   IS INITIAL.
       " ignore empty fields, already covered above
     ELSEIF <ls_travel>-enddate < <ls_travel>-startdate.

       APPEND CORRESPONDING #( <ls_travel> )
           TO failed-travel.
       APPEND VALUE #(
           %tky     = <ls_travel>-%tky
           %element = VALUE #( startdate = if_abap_behv=>mk-on
                               enddate   = if_abap_behv=>mk-on )
           %msg     = NEW Ycm_devs4d437(
                        textid   = Ycm_devs4d437=>dates_wrong_sequence
                        severity = Ycm_devs4d437=>severity-error
                                      )
            %state_area = c_area
                  )
           TO reported-travel.

     ENDIF.

   ENDLOOP.
 ENDMETHOD.

 METHOD determinesemantickey.

   DATA lt_travel_upd TYPE TABLE FOR UPDATE Yd437e_i_travel.

* get AgencyID for all new travels
**********************************************************************
   DATA(lv_agencyid) =
       Ycl_s4d437_model=>get_agency_by_user(
*         iv_user = SY-UNAME
*         iv_user = cl_abap_context_info=>get_user_technical_name(  )
                                          ).

* prepare input for MODIFY ENTITY
**********************************************************************
   lt_travel_upd = CORRESPONDING #(  keys ).

   LOOP AT lt_travel_upd ASSIGNING FIELD-SYMBOL(<ls_travel_upd>).

     <ls_travel_upd>-agencyid = lv_agencyid.
     <ls_travel_upd>-travelid =
          Ycl_s4d437_model=>get_next_travelid_for_agency(
                                          iv_agencynum = lv_agencyid
                                                       ).
   ENDLOOP.

* Update entities
**********************************************************************
   MODIFY ENTITY IN LOCAL MODE Yd437e_i_travel
    UPDATE FIELDS ( agencyid travelid )
    WITH lt_travel_upd
    REPORTED DATA(ls_reported).

   MOVE-CORRESPONDING ls_reported-travel
                   TO    reported-travel.


* Alternative with even more expression-based syntax
**********************************************************************
* get AgencyID for all new travels
*    DATA(lv_agencyid) =
*        cl_s4d437e_model=>get_agency_by_user( ).
*
*    MODIFY ENTITY IN LOCAL MODE d437e_i_travel
*     UPDATE FIELDS ( agencyid travelid )
*     WITH VALUE #( FOR key IN keys
*                    (
*                      %tky = key-%tky
*                      agencyid = lv_agencyid
*                      travelid =
*                       cl_s4d437e_model=>get_next_travelid_for_agency(
*                                    iv_agencynum = lv_agencyid
*                                                                    )
*                     )
*                   )
*     REPORTED DATA(ls_reported).
*
*    reported = CORRESPONDING #(  DEEP ls_reported ).

 ENDMETHOD.

 METHOD get_features.

* work area for parameter result
   DATA ls_result LIKE LINE OF result.

* helper objects to shorten the code
   CONSTANTS c_enabled   TYPE if_abap_behv=>t_xflag
                        VALUE if_abap_behv=>fc-o-enabled.
   CONSTANTS c_disabled  TYPE if_abap_behv=>t_xflag
                        VALUE if_abap_behv=>fc-o-disabled.
   CONSTANTS c_read_only TYPE if_abap_behv=>t_xflag
                        VALUE if_abap_behv=>fc-f-read_only.
   CONSTANTS c_mandatory TYPE if_abap_behv=>t_xflag
                        VALUE if_abap_behv=>fc-f-mandatory.

   DATA lv_today TYPE cl_abap_context_info=>ty_system_date.

**********************************************************************
* Get system date
   lv_today = cl_abap_context_info=>get_system_date( ).

* Read data of all affected
   READ ENTITY IN LOCAL MODE Yd437e_i_travel
        FIELDS ( status startdate enddate )
        WITH CORRESPONDING #( keys )
        RESULT DATA(lt_travel).

   LOOP AT lt_travel ASSIGNING FIELD-SYMBOL(<ls_travel>).

* for draft: distinguish between active and draft

     IF <ls_travel>-%is_draft = if_abap_behv=>mk-off.
       " active instance
       ASSIGN <ls_travel> TO FIELD-SYMBOL(<ls_for_check>).
     ELSE.
       " draft instance
       READ ENTITY IN LOCAL MODE Yd437e_i_travel
     FIELDS ( status startdate enddate )
     WITH VALUE #( ( %key      = <ls_travel>-%key
                     %is_draft = if_abap_behv=>mk-off
                   ) )
     RESULT DATA(lt_travel_active).

       IF lt_travel_active IS INITIAL.
         " new draft
         ASSIGN <ls_travel> TO <ls_for_check>.
       ELSE.
         " edit draft
         READ TABLE lt_travel_active INDEX 1 ASSIGNING <ls_for_check>.
       ENDIF.
     ENDIF.

* Transfer complete key to result table
     ls_result-%tky = <ls_travel>-%tky.

* Dynamic action control
     IF <ls_for_check>-status = 'C'.  "already cancelled
       ls_result-%features-%action-set_to_cancelled = c_disabled.
     ELSEIF <ls_for_check>-enddate IS NOT INITIAL
        AND <ls_for_check>-enddate <= lv_today.
       ls_result-%features-%action-set_to_cancelled = c_disabled.
     ELSE.
       ls_result-%features-%action-set_to_cancelled = c_enabled.
     ENDIF.

* dynamic operation control (udpdate)

     IF <ls_for_check>-status = 'C'.  "already cancelled
       ls_result-%features-%update = c_disabled.
     ELSEIF <ls_for_check>-enddate IS NOT INITIAL
        AND <ls_for_check>-enddate <= lv_today.
       ls_result-%features-%update = c_disabled.
     ELSE.
       ls_result-%features-%update = c_enabled.
     ENDIF.

* dynamic field control (Customer, StartDate)

     IF <ls_for_check>-startdate IS NOT INITIAL
        AND <ls_for_check>-startdate <= lv_today.
       ls_result-%features-%field-startdate  = c_read_only.
       ls_result-%features-%field-customerid = c_read_only.
     ELSE.
       ls_result-%features-%field-startdate  = c_mandatory.
       ls_result-%features-%field-customerid = c_mandatory.

     ENDIF.

     APPEND ls_result TO result.
   ENDLOOP.

* Alternative with even more expression-based syntax
**********************************************************************
*
*    DATA(lv_today) = cl_abap_context_info=>get_system_date(  ).
*
*    READ ENTITY IN LOCAL MODE d437e_i_travel
*         ALL FIELDS WITH CORRESPONDING #( keys )
*         RESULT DATA(lt_travel).
*
*    READ ENTITY IN LOCAL MODE d437e_i_travel
*         ALL FIELDS WITH VALUE #( FOR key IN keys
*                                  ( %key = key-%key
*                                    %is_draft = if_abap_behv=>mk-off
*                                  ) )
*         RESULT DATA(lt_travel_active)
*         FAILED DATA(ls_failed).
*
** for new drafts
*    result =
*       VALUE #(   FOR row IN ls_failed-travel
*                (
*                  %tky = keys[ KEY entity %key = row-%key ]-%tky
*                )
*              ).
*
** for edit draft and active
*    result =
*      VALUE #( BASE result
*               FOR <travel> IN lt_travel_active
*               (
*                 "key
*                 %tky = keys[ KEY entity %key = <travel>-%key ]-%tky
*                 "action control
*                 %features-%action-set_to_cancelled
*                    = COND #( WHEN <travel>-status = 'C'
*                                THEN if_abap_behv=>fc-o-disabled
*                              WHEN <travel>-enddate IS NOT INITIAL
*                               AND <travel>-enddate <= lv_today
*                                THEN if_abap_behv=>fc-o-disabled
*                              ELSE   if_abap_behv=>fc-o-enabled
*                            )
*                 "operation control
*                 %features-%update
*                    = COND #( WHEN <travel>-status = 'C'
*                                THEN if_abap_behv=>fc-o-disabled
*                              WHEN <travel>-enddate IS NOT INITIAL
*                               AND <travel>-enddate <= lv_today
*                              THEN if_abap_behv=>fc-o-disabled
*                              ELSE  if_abap_behv=>fc-o-enabled
*                            )
*                 "field control
*                 %features-%field-startdate
*                    = COND #(  WHEN <travel>-startdate IS NOT INITIAL
*                               AND <travel>-startdate <= lv_today
*                                 THEN  if_abap_behv=>fc-f-read_only
*                               ELSE    if_abap_behv=>fc-f-mandatory
*                            )
*                 %features-%field-customerid
*                    = COND #(  WHEN <travel>-startdate IS NOT INITIAL
*                                AND <travel>-startdate <= lv_today
*                                 THEN  if_abap_behv=>fc-f-read_only
*                               ELSE    if_abap_behv=>fc-f-mandatory
*                            )
*               )
*             ).

 ENDMETHOD.

ENDCLASS.
" 项目：
CLASS lhc_item DEFINITION INHERITING FROM cl_abap_behavior_handler.

 PRIVATE SECTION.

   METHODS determinesemantickey FOR DETERMINE ON MODIFY
     IMPORTING keys FOR item~determinesemantickey.
   METHODS validateflightclass FOR VALIDATE ON SAVE
     IMPORTING keys FOR item~validateflightclass.
   METHODS validateflightdate FOR VALIDATE ON SAVE
     IMPORTING keys FOR item~validateflightdate.
   METHODS validateflight FOR VALIDATE ON SAVE
     IMPORTING keys FOR item~validateflight.

ENDCLASS.

CLASS lhc_item IMPLEMENTATION.

 METHOD determinesemantickey.

* read semantic key data of all affected travel items
**********************************************************************
   READ ENTITY IN LOCAL MODE Yd437e_i_travelitem
              FIELDS ( agencyid travelid itemid )
              WITH CORRESPONDING #(  keys )
              RESULT DATA(lt_items).

* Loop over all affected travel items
**********************************************************************
   LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).

* Retrieve AgencyID and TravelID if not initial
**********************************************************************
     IF <ls_item>-agencyid IS INITIAL
     OR <ls_item>-travelid IS INITIAL.

       " Read parent entity data (travel)
       "for this child entity (travel item) by association
       READ ENTITY IN LOCAL MODE Yd437e_i_travelitem
                  BY \_travel FIELDS ( agencyid travelid )
                  WITH VALUE #( ( %tky = <ls_item>-%tky ) )
                  RESULT DATA(lt_travels).

       <ls_item>-agencyid = lt_travels[ 1 ]-agencyid.
       <ls_item>-travelid = lt_travels[ 1 ]-travelid.

     ENDIF.

* Retrieve ItemID if not initial
**********************************************************************
     IF <ls_item>-itemid IS INITIAL.

       " read all child entities (travel items)
       " assigned to the same parent entity (travel) by association

       READ ENTITY IN LOCAL MODE Yd437e_i_travel
                  BY \_travelitem FIELDS ( itemid )
                  WITH VALUE #(  ( %tky = lt_travels[ 1 ]-%tky ) )
                   RESULT DATA(lt_other_items).

       " find maximum item number
       LOOP AT lt_other_items ASSIGNING FIELD-SYMBOL(<ls_other_item>).

         IF <ls_other_item>-itemid > <ls_item>-itemid.
           <ls_item>-itemid = <ls_other_item>-itemid.
         ENDIF.
       ENDLOOP.
       "
       <ls_item>-itemid = <ls_item>-itemid + 10.
     ENDIF.

   ENDLOOP.

* Update flight travel items with new data
**********************************************************************
   MODIFY ENTITY IN LOCAL MODE Yd437e_i_travelitem
         UPDATE FIELDS ( agencyid travelid itemid )
         WITH CORRESPONDING #( lt_items ).


** Alternative 1: Bundled Read of BO data
***********************************************************************
*
*   READ ENTITIES OF  d437e_i_travel IN LOCAL MODE
*          ENTITY item
*          FIELDS (  agencyid travelid itemid trguid )
*          WITH   CORRESPONDING #(  keys )
*          RESULT DATA(lt_items)
*
*          ENTITY item BY \_travel
*          FIELDS ( agencyid travelid )
*          WITH   CORRESPONDING #(  keys )
*          RESULT DATA(lt_travels)
*          LINK   DATA(lt_link).
*
*   READ ENTITIES OF  d437e_i_travel IN LOCAL MODE
*          ENTITY travel BY \_travelitem
*          FIELDS (  trguid itemid )
*          WITH   VALUE #( FOR travel IN lt_travels
*                           ( %tky = travel-%tky )
*                         )
*          RESULT DATA(lt_other_items).
*
** Loop over all affected travel items
***********************************************************************
*
*    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
*
** Retrieve AgencyID and TravelID if not initial
***********************************************************************
*
*      IF <ls_item>-agencyid IS INITIAL
*      OR <ls_item>-travelid IS INITIAL.
*
*        read table lt_link assigning FIELD-SYMBOL(<ls_link>)
*              with key draft
*            components source-%tky = <ls_item>-%tky.
*
*        read table lt_travels assigning field-SYMBOL(<ls_travel>)
*              with key draft
*            components %tky = <ls_link>-target-%tky.
*
*        <ls_item>-agencyid = <ls_travel>-agencyid.
*        <ls_item>-travelid = <ls_travel>-travelid.
*
*      ENDIF.
*
** Retrieve ItemID if not initial
***********************************************************************
*      IF <ls_item>-itemid IS INITIAL.
*
*        LOOP AT lt_other_items ASSIGNING FIELD-SYMBOL(<ls_other_item>)
*                                   WHERE trguid = <ls_item>-trguid.
*
*          IF <ls_other_item>-itemid > <ls_item>-itemid.
*            <ls_item>-itemid = <ls_other_item>-itemid.
*          ENDIF.
*
*        ENDLOOP.
*
*        <ls_item>-itemid = <ls_item>-itemid + 10.
*
*      ENDIF.
*
*    ENDLOOP.
*
** Update flight travel items with new data
***********************************************************************
*
*    MODIFY ENTITY IN LOCAL MODE d437e_i_travelitem
*          UPDATE FIELDS ( agencyid travelid itemid )
*          WITH CORRESPONDING #( lt_items ).

**********************************************************************
** Alternative 2: Extreme use of  expression-based syntax
**********************************************************************
*
*    READ ENTITIES OF  d437e_i_travel IN LOCAL MODE
*           ENTITY item
*           FIELDS ( agencyid travelid itemid trguid )
*           WITH   CORRESPONDING #( keys )
*           RESULT DATA(lt_items)
*
*           ENTITY item BY \_travel
*           FIELDS ( agencyid travelid )
*           WITH   CORRESPONDING #(  keys )
*           RESULT DATA(lt_travels)
*           LINK   DATA(lt_link).
*
*    READ ENTITIES OF d437e_i_travel IN LOCAL MODE
*           ENTITY travel BY \_travelitem
*           FIELDS ( itemid trguid )
*           WITH   VALUE #( FOR travel IN lt_travels
*                            ( %tky = travel-%tky )
*                          )
*           RESULT DATA(lt_other_items).
*
*
*    MODIFY ENTITIES OF d437e_i_travel IN LOCAL MODE
*          ENTITY item
*          UPDATE FIELDS ( agencyid travelid itemid )
*          WITH VALUE #( FOR <item> IN lt_items
*             (  %tky = <item>-%tky
*            agencyid = COND #(
*                        WHEN <item>-agencyid IS INITIAL
*                        THEN lt_travels[ KEY draft
*                          COMPONENTS %tky = lt_link[
*                                 KEY draft
*                          COMPONENTS source-%tky = <item>-%tky
*                                                   ]-target-%tky
*                                      ]-agencyid
*                       ELSE <item>-agencyid
*                             )
*             travelid = COND #(
*                         WHEN <item>-travelid IS INITIAL
*                         THEN lt_travels[ KEY draft
*                           COMPONENTS %tky = lt_link[
*                                  KEY draft
*                           COMPONENTS source-%tky = <item>-%tky
*                                                    ]-target-%tky
*                                        ]-travelid
*                              )
*                itemid   = COND #(
*                            WHEN <item>-itemid IS INITIAL
*                            THEN 10
*                               + REDUCE #( INIT new_id = 0
*                                   FOR <other_item> IN lt_other_items
*                                 WHERE ( trguid = <item>-trguid )
*                                  NEXT new_id =
*                                       COND #(
*                                          WHEN <other_item>-itemid > new_id
*                                          THEN <other_item>-itemid
*                                          ELSE new_id
*                                             )
*                                          )
*                            ELSE <item>-itemid
*                                   )
*             )
*                     ).

 ENDMETHOD.

 METHOD validateflightclass.

   CONSTANTS c_area TYPE string VALUE `FLIGHTCLASS`.

   READ ENTITY IN LOCAL MODE Yd437e_i_travelitem
        FIELDS ( flightclass trguid )
        WITH CORRESPONDING #( keys )
        RESULT DATA(lt_items).

   LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).

     APPEND VALUE #( %tky    = <ls_item>-%tky
                     %state_area = c_area )
         TO reported-item.

     IF <ls_item>-flightclass IS INITIAL.

       APPEND CORRESPONDING #( <ls_item> )
           TO failed-item.
       APPEND VALUE #(
              %tky     = <ls_item>-%tky
              %element = VALUE #( flightclass = if_abap_behv=>mk-on )
              %msg     = NEW Ycm_devs4d437(
                           textid   = Ycm_devs4d437=>field_empty
                           severity = Ycm_devs4d437=>severity-error
                                        )
               %state_area = c_area
               %path       = VALUE #(  travel-%is_draft = <ls_item>-%is_draft
                                       travel-trguid    = <ls_item>-trguid )
                    )
           TO reported-item.

     ELSE.

       "existence check for flight class
       SELECT SINGLE @abap_true
                FROM Yd437_i_flightclass
               WHERE flightclass = @<ls_item>-flightclass
               INTO  @DATA(lv_exists)
               .

       IF lv_exists <> abap_true.

         APPEND CORRESPONDING #( <ls_item> )
             TO failed-item.
         APPEND VALUE #(
                %tky     = <ls_item>-%tky
                %element = VALUE #( flightclass = if_abap_behv=>mk-on )
                %msg     = NEW Ycm_devs4d437(
                             textid   = Ycm_devs4d437=>class_invalid
                             severity = Ycm_devs4d437=>severity-error
                             flightclass = <ls_item>-flightclass
                                           )
                %state_area = c_area
                %path       = VALUE #(  travel-%is_draft = <ls_item>-%is_draft
                                        travel-trguid    = <ls_item>-trguid )
                      )
             TO reported-item.

       ENDIF.
     ENDIF.

   ENDLOOP.

 ENDMETHOD.

 METHOD validateflightdate.

   CONSTANTS c_area TYPE string VALUE `FLIGHTDATE`.

   READ ENTITY IN LOCAL MODE Yd437e_i_travelitem
        FIELDS ( flightdate trguid )
        WITH CORRESPONDING #( keys )
        RESULT DATA(lt_items).

   LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).

     APPEND VALUE #( %tky    = <ls_item>-%tky
                     %state_area = c_area
                   )
         TO reported-item.

     IF <ls_item>-flightdate IS INITIAL.

       APPEND CORRESPONDING #( <ls_item> )
           TO failed-item.
       APPEND VALUE #(
              %tky     = <ls_item>-%tky
              %element = VALUE #( flightdate = if_abap_behv=>mk-on )
              %msg     = NEW Ycm_devs4d437(
                           textid   = Ycm_devs4d437=>field_empty
                           severity = Ycm_devs4d437=>severity-error
                                        )
                     %state_area = c_area
                     %path  = VALUE #(  travel-%is_draft = <ls_item>-%is_draft
                                        travel-trguid    = <ls_item>-trguid )
                   )
           TO reported-item.

     ELSEIF <ls_item>-flightdate < sy-datum.
       " or use cl_abap_context_info=>get_system_date( )

       APPEND CORRESPONDING #( <ls_item> )
           TO failed-item.
       APPEND VALUE #(
              %tky     = <ls_item>-%tky
              %element = VALUE #( flightdate = if_abap_behv=>mk-on )
              %msg     = NEW Ycm_devs4d437(
                           textid   = Ycm_devs4d437=>flight_date_past
                           severity = Ycm_devs4d437=>severity-error
                                         )
                     %state_area = c_area
                     %path  = VALUE #(  travel-%is_draft = <ls_item>-%is_draft
                                        travel-trguid    = <ls_item>-trguid )
                   )
           TO reported-item.

     ENDIF.

   ENDLOOP.

 ENDMETHOD.

 METHOD validateflight.

   CONSTANTS c_area TYPE string VALUE `FLIGHT`.

   READ ENTITY IN LOCAL MODE Yd437e_i_travelitem
        FIELDS ( carrierid connectionid flightdate trguid )
        WITH CORRESPONDING #( keys )
        RESULT DATA(lt_items).

   LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).

     APPEND VALUE #( %tky    = <ls_item>-%tky
                     %state_area = c_area )
         TO reported-item.

     IF <ls_item>-carrierid IS INITIAL.

       APPEND CORRESPONDING #( <ls_item> )
           TO failed-item.
       APPEND VALUE #(
              %tky     = <ls_item>-%tky
              %element = VALUE #( carrierid = if_abap_behv=>mk-on )
              %msg     = NEW Ycm_devs4d437(
                           textid   = Ycm_devs4d437=>field_empty
                           severity = Ycm_devs4d437=>severity-error
                                        )
               %state_area = c_area
               %path   = VALUE #(  travel-%is_draft = <ls_item>-%is_draft
                                   travel-trguid    = <ls_item>-trguid )
                     )
           TO reported-item.
     ELSEIF <ls_item>-connectionid IS INITIAL.

       APPEND CORRESPONDING #( <ls_item> )
           TO failed-item.
       APPEND VALUE #(
              %tky     = <ls_item>-%tky
              %element = VALUE #( connectionid = if_abap_behv=>mk-on )
              %msg     = NEW Ycm_devs4d437(
                           textid   = Ycm_devs4d437=>field_empty
                           severity = Ycm_devs4d437=>severity-error
                                        )
               %state_area = c_area
               %path   = VALUE #(  travel-%is_draft = <ls_item>-%is_draft
                                   travel-trguid    = <ls_item>-trguid )

                     )
           TO reported-item.

     ELSE.

       "existence check for flight
       SELECT SINGLE @abap_true
                FROM Yd437_i_flight
               WHERE carrierid = @<ls_item>-carrierid
                 AND connectionid = @<ls_item>-connectionid
                 AND flightdate       = @<ls_item>-flightdate
               INTO  @DATA(lv_exists)
               .

       IF lv_exists <> abap_true.

         APPEND CORRESPONDING #( <ls_item> )
             TO failed-item.
         APPEND VALUE #(
                %tky     = <ls_item>-%tky
                %element = VALUE #( flightclass = if_abap_behv=>mk-on )
                %msg     = NEW Ycm_devs4d437(
                             textid   = Ycm_devs4d437=>flight_not_exist
                             severity = Ycm_devs4d437=>severity-error
                             carrierid    = <ls_item>-carrierid
                             connectionid = <ls_item>-connectionid
                             flightdate   = <ls_item>-flightdate
                                           )
                %state_area = c_area
                %path    = VALUE #(  travel-%is_draft = <ls_item>-%is_draft
                                     travel-trguid    = <ls_item>-trguid )

                      )
             TO reported-item.

       ENDIF.
     ENDIF.

   ENDLOOP.

 ENDMETHOD.

ENDCLASS.
