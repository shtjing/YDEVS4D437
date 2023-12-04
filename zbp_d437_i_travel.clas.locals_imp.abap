CLASS lhc_YD437_I_TRAVEL DEFINITION INHERITING FROM CL_ABAP_BEHAVIOR_HANDLER.
  PRIVATE SECTION.

    METHODS SET_TO_CANCELLED FOR MODIFY
      IMPORTING KEYS FOR ACTION YD437_I_TRAVEL~SET_TO_CANCELLED.
    METHODS GET_INSTANCE_AUTHORIZATIONS FOR INSTANCE AUTHORIZATION
      IMPORTING KEYS REQUEST REQUESTED_AUTHORIZATIONS FOR YD437_I_TRAVEL RESULT RESULT.
    METHODS VALIDATECUSTOMER FOR VALIDATE ON SAVE
      IMPORTING KEYS FOR YD437_I_TRAVEL~validateCustomer.
    METHODS determineSemanticKey FOR DETERMINE ON MODIFY
      IMPORTING KEYS FOR YD437_I_TRAVEL~determineSemanticKey.
    METHODS GET_INSTANCE_FEATURES FOR FEATURES
      IMPORTING KEYS REQUEST REQUESTED_FEATURES FOR YD437_I_TRAVEL RESULT RESULT.

ENDCLASS.

CLASS lhc_YD437_I_TRAVEL IMPLEMENTATION.

  METHOD SET_TO_CANCELLED.

    DATA LO_MSG TYPE REF TO ZCM_D437_TRAVEL .

    DATA LS_REPORTED_TRAVEL LIKE LINE OF REPORTED-YD437_I_TRAVEL."behavior define 的名称

    DATA LT_READ_IN TYPE TABLE FOR READ IMPORT YD437_I_TRAVEL.
    DATA LS_READ_IN LIKE LINE OF LT_READ_IN.

    DATA LT_TRAVEL TYPE TABLE FOR READ RESULT YD437_I_TRAVEL.

    DATA LT_UPDATE TYPE TABLE FOR UPDATE YD437_I_TRAVEL.
    DATA LS_UPDATE LIKE LINE OF LT_UPDATE.

    DATA LS_FAILED TYPE RESPONSE FOR FAILED YD437_I_TRAVEL.

    FIELD-SYMBOLS <LS_KEY> LIKE LINE OF KEYS.
    FIELD-SYMBOLS <LS_TRAVEL> LIKE LINE OF LT_TRAVEL.

    DATA LS_KEY LIKE LINE OF KEYS.

    LOOP AT KEYS ASSIGNING <LS_KEY>.
      MOVE-CORRESPONDING <LS_KEY> TO LS_READ_IN.
      APPEND LS_READ_IN TO LT_READ_IN.
    ENDLOOP.

    READ ENTITY IN LOCAL MODE YD437_I_TRAVEL
        ALL FIELDS WITH LT_READ_IN
                 RESULT LT_TRAVEL.

*    READ ENTITIES OF d437b_i_travel IN LOCAL MODE
*      ENTITY travel
*        ALL FIELDS WITH lt_read_in
*                 RESULT lt_travel.

    LOOP AT LT_TRAVEL ASSIGNING <LS_TRAVEL>.

      IF <LS_TRAVEL>-STATUS = 'C'. "already cancelled
        CREATE OBJECT LO_MSG
          EXPORTING
            TEXTID   = ZCM_D437_TRAVEL=>ALREADY_CANCELLED
            SEVERITY = IF_ABAP_BEHV_MESSAGE=>SEVERITY-ERROR.

        LS_REPORTED_TRAVEL-%TKY = <LS_TRAVEL>-%TKY.
        LS_REPORTED_TRAVEL-%MSG = LO_MSG.

        APPEND LS_REPORTED_TRAVEL TO REPORTED-YD437_I_TRAVEL.

      ELSE.
        CLEAR LT_UPDATE.
        LS_UPDATE-%TKY   = <LS_TRAVEL>-%TKY.
        LS_UPDATE-STATUS = 'C'.

        APPEND LS_UPDATE TO LT_UPDATE.

* Update Status of travel not yet cancelled

        MODIFY ENTITY IN LOCAL MODE YD437_I_TRAVEL
           UPDATE FIELDS (  STATUS ) WITH LT_UPDATE
           FAILED LS_FAILED.

*        MODIFY ENTITIES OF d437b_i_travel IN LOCAL MODE
*          ENTITY travel
*             UPDATE FIELDS (  status ) WITH lt_update
*             FAILED ls_failed.


        IF LS_FAILED IS INITIAL.
* Success message
          CREATE OBJECT LO_MSG
            EXPORTING
              TEXTID   = ZCM_D437_TRAVEL=>CANCEL_SUCCESS
              SEVERITY = IF_ABAP_BEHV_MESSAGE=>SEVERITY-SUCCESS.

          LS_REPORTED_TRAVEL-%KEY = <LS_TRAVEL>-%KEY.
          LS_REPORTED_TRAVEL-%MSG = LO_MSG.

          APPEND LS_REPORTED_TRAVEL TO REPORTED-YD437_I_TRAVEL.

        ENDIF.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.

  METHOD GET_INSTANCE_AUTHORIZATIONS.
    "根据KEY读取值
    READ ENTITY IN LOCAL MODE YD437_I_TRAVEL
              FIELDS ( AGENCYID )  WITH CORRESPONDING #( KEYS )
                RESULT DATA(LT_TRAVEL).

    "权限检查，设置按钮事件的权限 例如：update action-set_to_cancelled
    LOOP AT LT_TRAVEL ASSIGNING FIELD-SYMBOL(<LS_TRAVEL>).

      DATA(LV_SUBRC) = Ycl_s4d437_model=>AUTHORITY_CHECK(
        EXPORTING
          IV_AGENCYNUM = <LS_TRAVEL>-AGENCYID
          IV_ACTVT     = '02'
      ).

      IF LV_SUBRC <> 0.

        APPEND VALUE #( %TKY = <LS_TRAVEL>-%TKY
                        %UPDATE = IF_ABAP_BEHV=>AUTH-UNAUTHORIZED
                        %ACTION-SET_TO_CANCELLED = IF_ABAP_BEHV=>AUTH-UNAUTHORIZED
                      )
             TO RESULT.


      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD validateCustomer.

    DATA LO_MSG TYPE REF TO ZCM_D437_TRAVEL.

* work areas for response parameters
    DATA LS_REPORTED_TRAVEL LIKE LINE OF REPORTED-YD437_I_TRAVEL.
    DATA LS_FAILED_TRAVEL   LIKE LINE OF FAILED-YD437_I_TRAVEL.

* read required data
**********************************************************************
    READ ENTITY IN LOCAL MODE YD437_I_TRAVEL
       FIELDS ( CUSTOMERID ) WITH CORRESPONDING #( KEYS )
       RESULT DATA(LT_TRAVEL).

    LOOP AT LT_TRAVEL ASSIGNING FIELD-SYMBOL(<LS_TRAVEL>).

* validate data and create message object in case of error
**********************************************************************

      IF <LS_TRAVEL>-CUSTOMERID IS INITIAL.
        "error because of initial input field
        CREATE OBJECT LO_MSG
          EXPORTING
            TEXTID   = ZCM_D437_TRAVEL=>FIELD_EMPTY
            SEVERITY = IF_ABAP_BEHV_MESSAGE=>SEVERITY-ERROR.

        " expression-based alternative
*        lo_msg = new #( textid   = cm_devs4d437=>field_empty
*                        severity = cm_devs4d437=>severity-error ).
      ELSE.
        "existence check for customer
        SELECT SINGLE @ABAP_TRUE
                 FROM Yd437_i_customer
                WHERE CUSTOMER = @<LS_TRAVEL>-CUSTOMERID
                 INTO @DATA(LV_EXISTS).


        IF LV_EXISTS <> ABAP_TRUE.
          " error because of non-existent customer
          CREATE OBJECT LO_MSG
            EXPORTING
              TEXTID   = ZCM_D437_TRAVEL=>CUSTOMER_NOT_EXIST
"             customerid = <ls_travel>-customerid
              SEVERITY = IF_ABAP_BEHV_MESSAGE=>SEVERITY-ERROR.

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
      IF LO_MSG IS BOUND.

        CLEAR LS_FAILED_TRAVEL.
        MOVE-CORRESPONDING <LS_TRAVEL> TO LS_FAILED_TRAVEL.
        APPEND LS_FAILED_TRAVEL TO FAILED-YD437_I_TRAVEL.

        CLEAR LS_REPORTED_TRAVEL.
        MOVE-CORRESPONDING <LS_TRAVEL> TO LS_REPORTED_TRAVEL.
        LS_REPORTED_TRAVEL-%ELEMENT-CUSTOMERID = IF_ABAP_BEHV=>MK-ON.
        LS_REPORTED_TRAVEL-%MSG = LO_MSG.
        APPEND LS_REPORTED_TRAVEL TO REPORTED-YD437_I_TRAVEL.

        " expression-based alternative without helper variables
*       APPEND CORRESPONDING #( <ls_travel> )
*           TO failed-Travel.
*
*       APPEND VALUE #(
*          %tky     = <ls_travel>-%tky
*          %element = VALUE #( customerid = if_abap_behv=>mk-on )
*          %msg     = NEW cm_devs4d437(
*                       textid     = cm_devs4d437=>customer_not_exist
*                       customerid = <ls_travel>-customerid
*                       severity   = cm_devs4d437=>severity-error
*                                 )
*                     )
*            TO reported-Travel.
*
        CLEAR LO_MSG.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD determineSemanticKey.
    DATA LT_TRAVEL_UPD TYPE TABLE FOR UPDATE YD437_I_TRAVEL.

* get AgencyID for all new travels
**********************************************************************
    DATA(LV_AGENCYID) =
        Ycl_s4d437_model=>GET_AGENCY_BY_USER(
*         iv_user = SY-UNAME
*         iv_user = cl_abap_context_info=>get_user_technical_name(  )
                                           ).

* prepare input for MODIFY ENTITY
**********************************************************************
    LT_TRAVEL_UPD = CORRESPONDING #(  KEYS ).

    LOOP AT LT_TRAVEL_UPD ASSIGNING FIELD-SYMBOL(<LS_TRAVEL_UPD>).

      <LS_TRAVEL_UPD>-AGENCYID = LV_AGENCYID.
      <LS_TRAVEL_UPD>-TRAVELID =
        Ycl_s4d437_model=>GET_NEXT_TRAVELID_FOR_AGENCY(
        IV_AGENCYNUM = LV_AGENCYID
      ).
    ENDLOOP.

* Update entities
**********************************************************************
    MODIFY ENTITY IN LOCAL MODE YD437_I_TRAVEL
     UPDATE FIELDS ( AGENCYID TRAVELID )
     WITH LT_TRAVEL_UPD
     REPORTED DATA(LS_REPORTED).

    MOVE-CORRESPONDING LS_REPORTED-YD437_I_TRAVEL
                    TO    REPORTED-YD437_I_TRAVEL.


* Alternative with even more expression-based syntax
**********************************************************************
* get AgencyID for all new travels
*    DATA(lv_agencyid) =
*        cl_s4d437c_model=>get_agency_by_user( ).
*
*    MODIFY ENTITY IN LOCAL MODE d437c_i_travel
*     UPDATE FIELDS ( agencyid travelid )
*     WITH VALUE #( FOR key IN keys
*                    (
*                      %tky = key-%tky
*                      agencyid = lv_agencyid
*                      travelid =
*                       cl_s4d437c_model=>get_next_travelid_for_agency(
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
    DATA LS_RESULT LIKE LINE OF RESULT.

* helper objects to shorten the code
    CONSTANTS C_ENABLED   TYPE IF_ABAP_BEHV=>T_XFLAG
                         VALUE IF_ABAP_BEHV=>FC-O-ENABLED.
    CONSTANTS C_DISABLED  TYPE IF_ABAP_BEHV=>T_XFLAG
                         VALUE IF_ABAP_BEHV=>FC-O-DISABLED.
    CONSTANTS C_READ_ONLY TYPE IF_ABAP_BEHV=>T_XFLAG
                         VALUE IF_ABAP_BEHV=>FC-F-READ_ONLY.
    CONSTANTS C_MANDATORY TYPE IF_ABAP_BEHV=>T_XFLAG
                         VALUE IF_ABAP_BEHV=>FC-F-MANDATORY.

    DATA LV_TODAY TYPE CL_ABAP_CONTEXT_INFO=>TY_SYSTEM_DATE.

**********************************************************************
* Get system date
    LV_TODAY = CL_ABAP_CONTEXT_INFO=>GET_SYSTEM_DATE( ).

* Read data of all affected
    READ ENTITY IN LOCAL MODE YD437_I_TRAVEL
         FIELDS ( STATUS STARTDATE ENDDATE )
         WITH CORRESPONDING #( KEYS )
         RESULT DATA(LT_TRAVEL).

    LOOP AT LT_TRAVEL ASSIGNING FIELD-SYMBOL(<LS_TRAVEL>).

      LS_RESULT-%TKY = <LS_TRAVEL>-%TKY.

* Dynamic action control
      IF <LS_TRAVEL>-STATUS = 'C'.  "already cancelled
        LS_RESULT-%FEATURES-%ACTION-SET_TO_CANCELLED = C_ENABLED. "C_DISABLED.
      ELSEIF <LS_TRAVEL>-ENDDATE IS NOT INITIAL
         AND <LS_TRAVEL>-ENDDATE <= LV_TODAY.
        LS_RESULT-%FEATURES-%ACTION-SET_TO_CANCELLED = C_DISABLED.
      ELSE.
        LS_RESULT-%FEATURES-%ACTION-SET_TO_CANCELLED = C_ENABLED.
      ENDIF.

* dynamic operation control (udpdate)

      IF <LS_TRAVEL>-STATUS = 'C'.  "already cancelled
        LS_RESULT-%FEATURES-%UPDATE = C_ENABLED."C_DISABLED.
      ELSEIF <LS_TRAVEL>-ENDDATE IS NOT INITIAL
         AND <LS_TRAVEL>-ENDDATE <= LV_TODAY.
        LS_RESULT-%FEATURES-%UPDATE = C_DISABLED.
      ELSE.
        LS_RESULT-%FEATURES-%UPDATE = C_ENABLED.
      ENDIF.

* dynamic field control (Customer, StartDate)

      IF <LS_TRAVEL>-STARTDATE IS NOT INITIAL
         AND <LS_TRAVEL>-STARTDATE <= LV_TODAY.
        LS_RESULT-%FEATURES-%FIELD-STARTDATE  = C_READ_ONLY.
        LS_RESULT-%FEATURES-%FIELD-CUSTOMERID = C_READ_ONLY.
      ELSE.
        LS_RESULT-%FEATURES-%FIELD-STARTDATE  = C_MANDATORY.
        LS_RESULT-%FEATURES-%FIELD-CUSTOMERID = C_MANDATORY.

      ENDIF.

      APPEND LS_RESULT TO RESULT.
    ENDLOOP.

* Alternative with even more expression-based syntax
**********************************************************************
*
*    DATA(lv_today) = cl_abap_context_info=>get_system_date(  ).
*
*    READ ENTITY IN LOCAL MODE d437c_i_travel
*         ALL FIELDS WITH CORRESPONDING #( keys )
*         RESULT DATA(lt_travel).
*
*    result =
*      VALUE #( FOR <travel> IN lt_travel
*               (
*                 "key
*                 %tky = <travel>-%tky
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
