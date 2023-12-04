CLASS lhc_Travel DEFINITION INHERITING FROM CL_ABAP_BEHAVIOR_HANDLER.
  PRIVATE SECTION.

    METHODS GET_INSTANCE_FEATURES FOR FEATURES
      IMPORTING KEYS REQUEST REQUESTED_FEATURES FOR Travel RESULT RESULT.

    METHODS GET_INSTANCE_AUTHORIZATIONS FOR AUTHORIZATION
      IMPORTING KEYS REQUEST REQUESTED_AUTHORIZATIONS FOR Travel RESULT RESULT.

    METHODS ISSUE_MESSAGE FOR MODIFY
      IMPORTING KEYS FOR ACTION Travel~ISSUE_MESSAGE.

    METHODS SET_TO_CANCELLED FOR MODIFY
      IMPORTING KEYS FOR ACTION Travel~SET_TO_CANCELLED.

    METHODS determineSemanticKey FOR DETERMINE ON MODIFY
      IMPORTING KEYS FOR Travel~determineSemanticKey.

    METHODS validateCustomer FOR VALIDATE ON SAVE
      IMPORTING KEYS FOR Travel~validateCustomer.

    METHODS validateEndDate FOR VALIDATE ON SAVE
      IMPORTING KEYS FOR Travel~validateEndDate.

    METHODS validateSequence FOR VALIDATE ON SAVE
      IMPORTING KEYS FOR Travel~validateSequence.

    METHODS validateStartDate FOR VALIDATE ON SAVE
      IMPORTING KEYS FOR Travel~validateStartDate.

ENDCLASS.

CLASS lhc_Travel IMPLEMENTATION.

  METHOD GET_INSTANCE_AUTHORIZATIONS.
  ENDMETHOD.

  METHOD ISSUE_MESSAGE.
    DATA LO_MSG TYPE REF TO Ycm_d437d_travel.

    CREATE OBJECT LO_MSG
      EXPORTING
        TEXTID   = Ycm_d437d_travel=>CM_D437D_TRAVEL
        SEVERITY = Ycm_d437d_travel=>IF_ABAP_BEHV_MESSAGE~SEVERITY-SUCCESS.

    APPEND LO_MSG TO REPORTED-%OTHER.
  ENDMETHOD.

  METHOD SET_TO_CANCELLED.
    DATA LO_MSG TYPE REF TO Zcm_d437_TRAVEL.

    DATA LS_REPORTED_TRAVEL LIKE LINE OF REPORTED-TRAVEL.

    DATA LT_READ_IN TYPE TABLE FOR READ IMPORT Yd437d_i_travel.
    DATA LS_READ_IN LIKE LINE OF LT_READ_IN.

    DATA LT_TRAVEL TYPE TABLE FOR READ RESULT Yd437d_i_travel.

    DATA LT_UPDATE TYPE TABLE FOR UPDATE Yd437d_i_travel.
    DATA LS_UPDATE LIKE LINE OF LT_UPDATE.

    DATA LS_FAILED TYPE RESPONSE FOR FAILED Yd437d_i_travel.

    FIELD-SYMBOLS <LS_KEY> LIKE LINE OF KEYS.
    FIELD-SYMBOLS <LS_TRAVEL> LIKE LINE OF LT_TRAVEL.

    DATA LS_KEY LIKE LINE OF KEYS.

    LOOP AT KEYS ASSIGNING <LS_KEY>.
      MOVE-CORRESPONDING <LS_KEY> TO LS_READ_IN.
      APPEND LS_READ_IN TO LT_READ_IN.
    ENDLOOP.

    READ ENTITY IN LOCAL MODE Yd437d_i_travel
        ALL FIELDS WITH LT_READ_IN
        RESULT LT_TRAVEL.

    LOOP AT LT_TRAVEL ASSIGNING <LS_TRAVEL>.

      IF <LS_TRAVEL>-STATUS = 'C'. "already cancelled
        CREATE OBJECT LO_MSG
          EXPORTING
            TEXTID   = Ycm_d437d_travel=>ALREADY_CANCELLED
            SEVERITY = IF_ABAP_BEHV_MESSAGE=>SEVERITY-ERROR.

        LS_REPORTED_TRAVEL-%TKY = <LS_TRAVEL>-%TKY.
        LS_REPORTED_TRAVEL-%MSG = LO_MSG.

        APPEND LS_REPORTED_TRAVEL TO REPORTED-TRAVEL.

      ELSE.
        CLEAR LT_UPDATE.
        LS_UPDATE-%TKY   = <LS_TRAVEL>-%TKY.
        LS_UPDATE-STATUS = 'C'.

        APPEND LS_UPDATE TO LT_UPDATE.

* Update Status of travel not yet cancelled

        MODIFY ENTITY IN LOCAL MODE Yd437d_i_travel
           UPDATE FIELDS (  STATUS ) WITH LT_UPDATE
           FAILED LS_FAILED.

        IF LS_FAILED IS INITIAL.
* Success message
          CREATE OBJECT LO_MSG
            EXPORTING
              TEXTID   = Ycm_d437d_travel=>CANCEL_SUCCESS
              SEVERITY = IF_ABAP_BEHV_MESSAGE=>SEVERITY-SUCCESS.

          LS_REPORTED_TRAVEL-%KEY = <LS_TRAVEL>-%KEY.
          LS_REPORTED_TRAVEL-%MSG = LO_MSG.

          APPEND LS_REPORTED_TRAVEL TO REPORTED-TRAVEL.

        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD validateCustomer.
* for message object
   DATA lo_msg TYPE REF TO YCM_DEVS4D437.

* work areas for response parameters
   DATA ls_reported_travel LIKE LINE OF reported-travel.
   DATA ls_failed_travel   LIKE LINE OF failed-travel.

* Constant for state area (needed for validation messages in draft)

   CONSTANTS c_state TYPE string VALUE `CUSTOMER`.

* read required data
**********************************************************************
   READ ENTITY IN LOCAL MODE Yd437d_i_travel
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
           textid   = YCM_DEVS4D437=>field_empty
           severity = IF_ABAP_BEHV_MESSAGE=>severity-error.

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
             textid     = Zcm_d437_TRAVEL=>customer_not_exist
             customerid = <ls_travel>-customerid
             severity   = IF_ABAP_BEHV_MESSAGE=>severity-error.

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

   READ ENTITY IN LOCAL MODE Yd437d_i_travel
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
              %msg     = NEW YCM_DEVS4D437(
                           textid   = YCM_DEVS4D437=>field_empty
                           severity = YCM_DEVS4D437=>severity-error
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
              %msg     = NEW YCM_DEVS4D437(
                           textid   = YCM_DEVS4D437=>start_date_past
                           severity = YCM_DEVS4D437=>severity-error
                                         )
              %state_area = c_area
                    )
           TO reported-travel.

     ENDIF.

   ENDLOOP.
 ENDMETHOD.

 METHOD validateenddate.

   CONSTANTS c_area TYPE string VALUE `ENDDATE`.

   READ ENTITY IN LOCAL MODE Yd437d_i_travel
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
              %msg     = NEW YCM_DEVS4D437(
                             textid   = YCM_DEVS4D437=>field_empty
                             severity = YCM_DEVS4D437=>severity-error
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
              %msg     = NEW YCM_DEVS4D437(
                             textid   = YCM_DEVS4D437=>end_date_past
                             severity = YCM_DEVS4D437=>severity-error
                                         )
              %state_area = c_area
                     )
           TO reported-travel.

     ENDIF.

   ENDLOOP.
 ENDMETHOD.

 METHOD validatesequence.

   CONSTANTS c_area TYPE string VALUE `SEQUENCE`.

   READ ENTITY IN LOCAL MODE Yd437d_i_travel
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
           %msg     = NEW YCM_DEVS4D437(
                        textid   = YCM_DEVS4D437=>dates_wrong_sequence
                        severity = YCM_DEVS4D437=>severity-error
                                      )
            %state_area = c_area
                  )
           TO reported-travel.

     ENDIF.

   ENDLOOP.
 ENDMETHOD.

 METHOD determinesemantickey.

   DATA lt_travel_upd TYPE TABLE FOR UPDATE Yd437d_i_travel.

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
   MODIFY ENTITY IN LOCAL MODE Yd437d_i_travel
    UPDATE FIELDS ( agencyid travelid )
    WITH lt_travel_upd
    REPORTED DATA(ls_reported).

   MOVE-CORRESPONDING ls_reported-travel
                   TO    reported-travel.


* Alternative with even more expression-based syntax
**********************************************************************
* get AgencyID for all new travels
*    DATA(lv_agencyid) =
*        cl_s4d437d_model=>get_agency_by_user( ).
*
*    MODIFY ENTITY IN LOCAL MODE Yd437d_i_travel
*     UPDATE FIELDS ( agencyid travelid )
*     WITH VALUE #( FOR key IN keys
*                    (
*                      %tky = key-%tky
*                      agencyid = lv_agencyid
*                      travelid =
*                       cl_s4d437d_model=>get_next_travelid_for_agency(
*                                    iv_agencynum = lv_agencyid
*                                                                    )
*                     )
*                   )
*     REPORTED DATA(ls_reported).
*
*    reported = CORRESPONDING #(  DEEP ls_reported ).

 ENDMETHOD.

 METHOD GET_INSTANCE_FEATURES.

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
   READ ENTITY IN LOCAL MODE Yd437d_i_travel
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
       READ ENTITY IN LOCAL MODE Yd437d_i_travel
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
*    READ ENTITY IN LOCAL MODE Yd437d_i_travel
*         ALL FIELDS WITH CORRESPONDING #( keys )
*         RESULT DATA(lt_travel).
*
*    READ ENTITY IN LOCAL MODE Yd437d_i_travel
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
