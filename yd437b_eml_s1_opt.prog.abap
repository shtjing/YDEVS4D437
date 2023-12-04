*&---------------------------------------------------------------------*
*& Report YD437B_EML_S1_OPT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT YD437B_EML_S1_OPT MESSAGE-ID Ydevs4d437.



PARAMETERS PA_GUID TYPE Y_TRGUID
               DEFAULT '00000000000000000000000000000000'.
PARAMETERS PA_STAT TYPE Y_STATUS VALUE CHECK.

* Data declarations for read access

* Data declarations for update access

* Data declarations for response

START-OF-SELECTION.

* Read the RAP BO entity to check for current status
*---------------------------------------------------*

  READ ENTITY YD437_I_TRAVEL
  ALL FIELDS WITH VALUE #( ( %TKY-TRGUID = PA_GUID ) )
  RESULT   DATA(GT_READ_RESULT)
  FAILED   DATA(GS_FAILED).

  IF GS_FAILED IS NOT INITIAL.

    MESSAGE E103 WITH PA_GUID.

  ELSE.

    IF GT_READ_RESULT[ 1 ]-STATUS = PA_STAT.
      MESSAGE E110 WITH PA_GUID PA_STAT.
    ENDIF.

  ENDIF.


* Update RAP BO with new status
*-------------------------------*

  MODIFY ENTITY YD437_I_TRAVEL
        UPDATE FIELDS ( STATUS )
        WITH VALUE #( ( %TKY-TRGUID = PA_GUID
                        STATUS      = PA_STAT ) )
        FAILED GS_FAILED.

  IF GS_FAILED IS NOT INITIAL.
    MESSAGE E102 WITH PA_GUID.
  ELSE.

    COMMIT ENTITIES.

    WRITE: / 'Status of instance',  PA_GUID,
             'successfully set to', PA_STAT.
  ENDIF.
