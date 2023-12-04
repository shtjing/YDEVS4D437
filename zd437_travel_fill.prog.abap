*&---------------------------------------------------------------------*
*& Report ZD437_TRAVEL_FILL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZD437_TRAVEL_FILL.


CONSTANTS C_TEMPLATE TYPE TABNAME VALUE 'YD437_TRAVEL'.
TYPES     TT_DATA TYPE STANDARD TABLE OF yd437_travel
         WITH NON-UNIQUE DEFAULT KEY.

PARAMETERS PA_TAB TYPE DD03L-TABNAME VALUE CHECK.

CLASS LCL_DBTABLE DEFINITION.

  PUBLIC SECTION.

    DATA MV_TABNAME TYPE  TABNAME READ-ONLY.

    METHODS CONSTRUCTOR IMPORTING IV_TABNAME TYPE TABNAME.

    METHODS IS_EMPTY      RETURNING VALUE(RV_RESULT) TYPE ABAP_BOOL.
    METHODS HAS_GUID_FIELD RETURNING VALUE(RV_RESULT) TYPE ABAP_BOOL.

    METHODS CHECK_COMPATIBLE RETURNING VALUE(RV_CONSISTENT) TYPE ABAP_BOOL.

    METHODS GENERATE_DATA.

    METHODS UPDATE_WITH_GUID.

    METHODS DELETE_DATA.

  PRIVATE SECTION.

    CONSTANTS C_USER TYPE Y_CHANGEDBY VALUE 'GENERATOR'.

    DATA MR_DATA TYPE REF TO DATA.

    DATA MT_COMPONENTS TYPE ABAP_COMPDESCR_TAB.

    DATA MV_GUID_FIELD TYPE ABAP_COMPNAME.

    DATA MV_TOTETAG_FIELD TYPE ABAP_COMPNAME.

    METHODS ADD_GUID_FIELD.

    METHODS ADD_TOTETAG_FIELD.

    METHODS DBSELECT.

    METHODS DBSYNC.

ENDCLASS.

CLASS LCL_DBTABLE IMPLEMENTATION.

  METHOD CONSTRUCTOR.

    MV_TABNAME  = IV_TABNAME.
    MT_COMPONENTS = CAST CL_ABAP_STRUCTDESCR(
                         CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( IV_TABNAME )
                                           )->COMPONENTS.

    TRY.
        MV_GUID_FIELD = MT_COMPONENTS[ TYPE_KIND = CL_ABAP_TYPEDESCR=>TYPEKIND_HEX
                                       LENGTH    = 16  ]-NAME.

      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
    ENDTRY.

    IF MT_COMPONENTS[ LINES( MT_COMPONENTS ) ]-TYPE_KIND <> CL_ABAP_TYPEDESCR=>TYPEKIND_CHAR.
      MV_TOTETAG_FIELD = MT_COMPONENTS[ LINES( MT_COMPONENTS ) ]-NAME.
    ENDIF.

    CREATE DATA MR_DATA TYPE TABLE OF (MV_TABNAME).

    DBSELECT( ).

  ENDMETHOD.


  METHOD CHECK_COMPATIBLE.
    DATA(LO_TEMPLATE) = CAST CL_ABAP_STRUCTDESCR( CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( C_TEMPLATE ) ).

    DATA(LT_COMPONENTS) = MT_COMPONENTS.

* remove guid field if any
*    IF mv_guid_field IS NOT INITIAL.
*      DELETE lt_components WHERE name = mv_guid_field.
*    ENDIF.

* check for compatibility of type

* remove local last changed at field if any
*    IF lines(  lt_components ) = lines( lo_template->components ) + 1.
*      DELETE lt_components INDEX lines( lt_components ).
*    ENDIF.
    IF MV_TOTETAG_FIELD IS NOT INITIAL.
      DELETE LT_COMPONENTS WHERE NAME = MV_TOTETAG_FIELD.
    ENDIF.

    RV_CONSISTENT = ABAP_TRUE.

    IF LT_COMPONENTS <> LO_TEMPLATE->COMPONENTS.
      RV_CONSISTENT = ABAP_FALSE.
    ENDIF.
  ENDMETHOD.

  METHOD IS_EMPTY.
    RV_RESULT = ABAP_TRUE.
    SELECT SINGLE FROM (PA_TAB)
            FIELDS @ABAP_FALSE
            INTO @RV_RESULT.
  ENDMETHOD.

  METHOD HAS_GUID_FIELD.

    IF MV_GUID_FIELD IS NOT INITIAL.
      RV_RESULT = ABAP_TRUE.
    ELSE.
      RV_RESULT = ABAP_FALSE.
    ENDIF.

  ENDMETHOD.

  METHOD GENERATE_DATA.

    FIELD-SYMBOLS
       <LT_DATA> TYPE ANY TABLE.

    DATA(LV_AGENCY)   =  YCL_S4D437_MODEL=>GET_AGENCY_BY_USER( SY-UNAME ).
    GET TIME STAMP FIELD DATA(LV_CHANGED_AT) .
    DATA(LT_DATA) = VALUE TT_DATA(

* Travel in the past
     (
       AGENCYNUM  = LV_AGENCY
       TRAVELID   = YCL_S4D437_MODEL=>GET_NEXT_TRAVELID_FOR_AGENCY( LV_AGENCY )
       TRDESC     = 'Travel in the past'
       CUSTOMID   = 1
       STDAT      = SY-DATUM - 28
       ENDDAT     = SY-DATUM - 14
       STATUS     = ' '
       CHANGED_AT = LV_CHANGED_AT
       CHANGED_BY = C_USER
     )

* ongoing travel
     (
          AGENCYNUM = LV_AGENCY
          TRAVELID  = YCL_S4D437_MODEL=>GET_NEXT_TRAVELID_FOR_AGENCY( LV_AGENCY )
          TRDESC    = 'Travel ongoing'
          CUSTOMID  = 2
          STDAT     = SY-DATUM - 7
          ENDDAT    = SY-DATUM + 7
          STATUS    = ' '
          CHANGED_AT = LV_CHANGED_AT
          CHANGED_BY = C_USER

     )

* travel in the future

     (
          AGENCYNUM = LV_AGENCY
          TRAVELID  = YCL_S4D437_MODEL=>GET_NEXT_TRAVELID_FOR_AGENCY( LV_AGENCY )
          TRDESC    = 'Travel in the future'
          CUSTOMID  = 3
          STDAT     = SY-DATUM + 7
          ENDDAT    = SY-DATUM + 21
          STATUS    = ' '
          CHANGED_AT = LV_CHANGED_AT
          CHANGED_BY = C_USER
     )

* travel for travel agency 061
          (
          AGENCYNUM = '061'
          TRAVELID  = YCL_S4D437_MODEL=>GET_NEXT_TRAVELID_FOR_AGENCY( '061' )
          TRDESC    = 'Travel of travel agency 061'
          CUSTOMID  = 4
          STDAT     = SY-DATUM + 7
          ENDDAT    = SY-DATUM + 14
          STATUS    = ' '
          CHANGED_AT = LV_CHANGED_AT
          CHANGED_BY = C_USER
          )

* travel for travel agency 325
          (
          AGENCYNUM = '325'
          TRAVELID  = YCL_S4D437_MODEL=>GET_NEXT_TRAVELID_FOR_AGENCY( '325' )
          TRDESC    = 'Travel of agency 325'
          CUSTOMID  = 4
          STDAT     = SY-DATUM + 14
          ENDDAT    = SY-DATUM + 21
          STATUS    = ' '
          CHANGED_AT = LV_CHANGED_AT
          CHANGED_BY = C_USER
          )

    ).

    ASSIGN MR_DATA->* TO <LT_DATA>.

    MOVE-CORRESPONDING LT_DATA TO <LT_DATA>.

    IF MV_GUID_FIELD IS NOT INITIAL.
      ADD_GUID_FIELD( ).
    ENDIF.

    IF MV_TOTETAG_FIELD IS NOT INITIAL.
      ADD_TOTETAG_FIELD( ).
    ENDIF.


    DBSYNC( ).

  ENDMETHOD.

  METHOD UPDATE_WITH_GUID.

    IF MV_GUID_FIELD IS NOT INITIAL.
      ADD_GUID_FIELD( ).
      DBSYNC( ).

    ENDIF.
  ENDMETHOD.

  METHOD DELETE_DATA.
    FIELD-SYMBOLS
    <LT_DATA> TYPE ANY TABLE.

    ASSIGN MR_DATA->* TO <LT_DATA>.

    CLEAR <LT_DATA>.

    DBSYNC( ).

  ENDMETHOD.


  METHOD ADD_GUID_FIELD.
    FIELD-SYMBOLS
    <LT_DATA> TYPE ANY TABLE.

    ASSIGN MR_DATA->* TO <LT_DATA>.

    LOOP AT <LT_DATA> ASSIGNING FIELD-SYMBOL(<LS_DATA>).

      ASSIGN COMPONENT MV_GUID_FIELD OF STRUCTURE <LS_DATA> TO FIELD-SYMBOL(<LV_GUID>).

      IF <LV_GUID> IS INITIAL.

        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            EV_GUID_16 = <LV_GUID>.

        ASSIGN COMPONENT 'changed_at' OF STRUCTURE <LS_DATA> TO FIELD-SYMBOL(<LV_CHANGED_AT>).
        IF SY-SUBRC = 0.
          GET TIME STAMP FIELD <LV_CHANGED_AT>.
        ENDIF.

        ASSIGN COMPONENT 'changed_by' OF STRUCTURE <LS_DATA> TO FIELD-SYMBOL(<LV_CHANGED_BY>).
        IF SY-SUBRC = 0.
          <LV_CHANGED_BY> = C_USER.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD ADD_TOTETAG_FIELD.

    FIELD-SYMBOLS
    <LT_DATA> TYPE ANY TABLE.

    ASSIGN MR_DATA->* TO <LT_DATA>.

    LOOP AT <LT_DATA> ASSIGNING FIELD-SYMBOL(<LS_DATA>).

      ASSIGN COMPONENT MV_TOTETAG_FIELD OF STRUCTURE <LS_DATA> TO FIELD-SYMBOL(<LV_ETAG>).

      IF <LV_ETAG> IS INITIAL.
        GET TIME STAMP FIELD <LV_ETAG>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD DBSELECT.
    FIELD-SYMBOLS <LT_DATA> TYPE ANY TABLE.
    ASSIGN MR_DATA->* TO <LT_DATA>.

    SELECT * FROM (MV_TABNAME) INTO TABLE <LT_DATA>.

  ENDMETHOD.
  METHOD DBSYNC.
    FIELD-SYMBOLS <LT_DATA> TYPE ANY TABLE.

    ASSIGN MR_DATA->* TO <LT_DATA>.

    IF <LT_DATA> IS INITIAL.
      DELETE FROM (MV_TABNAME).
    ELSEIF IS_EMPTY( ) = ABAP_TRUE.

      INSERT (MV_TABNAME) FROM TABLE <LT_DATA>.

    ELSE.

      UPDATE (MV_TABNAME) FROM TABLE <LT_DATA>.

    ENDIF.

  ENDMETHOD.


ENDCLASS.


CLASS LCL_ALV_DISPLAY DEFINITION.

  PUBLIC SECTION.

    METHODS CONSTRUCTOR
      IMPORTING
        IO_DBTABLE TYPE REF TO LCL_DBTABLE.

    METHODS DISPLAY.

  PRIVATE SECTION.

    DATA MO_DBTABLE TYPE REF TO LCL_DBTABLE.

    DATA MO_SALV TYPE REF TO IF_SALV_GUI_TABLE_IDA.

    METHODS CONFIGURE_TOOLBAR.

    METHODS ON_FUNCTION_SELECTED
      FOR EVENT FUNCTION_SELECTED
      OF IF_SALV_GUI_TOOLBAR_IDA
      IMPORTING EV_FCODE.


ENDCLASS.


CLASS LCL_ALV_DISPLAY IMPLEMENTATION.

  METHOD CONSTRUCTOR.

    MO_DBTABLE = IO_DBTABLE.

    MO_SALV = CL_SALV_GUI_TABLE_IDA=>CREATE(
      IV_TABLE_NAME = MO_DBTABLE->MV_TABNAME
*     io_gui_container      =
    ).

    CONFIGURE_TOOLBAR( ).

  ENDMETHOD.

  METHOD CONFIGURE_TOOLBAR.

    DATA(LO_TOOLBAR) = MO_SALV->TOOLBAR( ).

    LO_TOOLBAR->HIDE_ALL_STANDARD_FUNCTIONS( ).

    LO_TOOLBAR->ADD_BUTTON(
      EXPORTING
        IV_FCODE = 'DELETE'
*       iv_icon  =                  " Name of an Icon
*       iv_is_disabled               = mo_dbtable->is_empty( )
        IV_TEXT  = 'Delete All'
*       iv_quickinfo                 =
    ).

    LO_TOOLBAR->ADD_BUTTON(
      EXPORTING
        IV_FCODE = 'GENERATE'
*       iv_icon  =                  " Name of an Icon
*       iv_is_disabled               = SWITCH #( mo_dbtable->is_empty( )
*                                             WHEN abap_true THEN abap_false
*                                             WHEN abap_false THEN abap_true )
        IV_TEXT  = 'Generate Data'
*       iv_quickinfo                 =
    ).

**   IF mo_dbtable->has_guid_field( ) = abap_true.
*    lo_toolbar->add_button(
*    EXPORTING
*      iv_fcode                     = 'GUID'
**    iv_icon                      =                  " Name of an Icon
**    iv_is_disabled               =
*      iv_text                      = 'Generate GUID'
**    iv_quickinfo                 =
*      ).
*
**    ENDIF.

    SET HANDLER ME->ON_FUNCTION_SELECTED FOR LO_TOOLBAR.

  ENDMETHOD.

  METHOD DISPLAY.

    MO_SALV->FULLSCREEN( )->DISPLAY( ).

  ENDMETHOD.

  METHOD ON_FUNCTION_SELECTED.

    CASE EV_FCODE.
      WHEN 'DELETE'.

        IF MO_DBTABLE->IS_EMPTY( ) <> ABAP_TRUE.
          MO_DBTABLE->DELETE_DATA( ).
        ELSE.
          MESSAGE I015(00).
        ENDIF.

      WHEN 'GENERATE'.

        IF MO_DBTABLE->IS_EMPTY( ) = ABAP_TRUE.
          MO_DBTABLE->GENERATE_DATA( ).
        ELSE.
          MESSAGE I020(00).
        ENDIF.

      WHEN 'GUID'.

        IF MO_DBTABLE->HAS_GUID_FIELD( ) = ABAP_TRUE.
          MO_DBTABLE->UPDATE_WITH_GUID( ).
        ELSE.
          MESSAGE I025(00).
        ENDIF.


    ENDCASE.

    MO_SALV->REFRESH( ).
*    me->configure_toolbar( ).
  ENDMETHOD.

ENDCLASS.


DATA GO_DBTABLE TYPE REF TO LCL_DBTABLE.

DATA GO_ALV_DISPLAY TYPE REF TO LCL_ALV_DISPLAY.

INITIALIZATION.

*  IF sy-uname CP 'TRAIN-++'.
  IF MATCHES( VAL = SY-UNAME
              REGEX = `TRAIN-\d{2}` ).
    PA_TAB = |Z{ SY-UNAME+6(2) }_TRAVEL|.
  ELSE.
    PA_TAB = 'Z00_TRAVEL'.
  ENDIF.


AT SELECTION-SCREEN.

  SELECT SINGLE FROM DD02L
    FIELDS @ABAP_TRUE
     WHERE TABNAME = @PA_TAB
  AND TABCLASS = 'TRANSP'
  AND AS4LOCAL = 'A'
    INTO @DATA(GV_EXISTS).

  IF GV_EXISTS <> ABAP_TRUE.
    MESSAGE E005(00) WITH PA_TAB.
  ENDIF.

  GO_DBTABLE = NEW #( IV_TABNAME = PA_TAB ).

  IF GO_DBTABLE->CHECK_COMPATIBLE( ) <> ABAP_TRUE.
    MESSAGE E010(00) WITH PA_TAB.
  ENDIF.

START-OF-SELECTION.

  GO_ALV_DISPLAY = NEW #( IO_DBTABLE = GO_DBTABLE ).

  GO_ALV_DISPLAY->DISPLAY( ).
