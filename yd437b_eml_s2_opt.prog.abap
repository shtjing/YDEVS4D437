*&---------------------------------------------------------------------*
*& Report YD437B_EML_S2_OPT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT YD437B_EML_S2_OPT MESSAGE-ID devs4d437.

PARAMETERS pa_guid TYPE Y_trguid
                  DEFAULT '00000000000000000000000000000000'.
PARAMETERS pa_stat TYPE Y_status VALUE CHECK.

* Data declarations for read access

* Data declarations for update access

* Data declarations for response

START-OF-SELECTION.

* Read the RAP BO entity to check for current status
*---------------------------------------------------*

*  READ ENTITY d437b_i_travel
*    ALL FIELDS WITH VALUE #( ( %tky-trguid = pa_guid ) )
*    RESULT   DATA(gt_read_result)
*    FAILED   DATA(gs_failed).

 READ ENTITIES OF YD437_I_TRAVEL
   ENTITY YD437_I_TRAVEL
     ALL FIELDS WITH VALUE #( ( %tky-trguid = pa_guid ) )
     RESULT   DATA(gt_read_result)
     FAILED   DATA(gs_failed).

 IF gs_failed IS NOT INITIAL.

   MESSAGE e103 WITH pa_guid.

 ELSE.

   IF gt_read_result[ 1 ]-status = pa_stat.
     MESSAGE e110 WITH pa_guid pa_stat.
   ENDIF.

 ENDIF.


* Update RAP BO with new status
*-------------------------------*

*  MODIFY ENTITY d437b_i_travel
*        UPDATE FIELDS ( status )
*        WITH VALUE #( ( %tky-trguid = pa_guid
*                        status      = pa_stat ) )
*        FAILED gs_failed.

 MODIFY ENTITIES OF YD437_I_TRAVEL
    ENTITY YD437_I_TRAVEL
       UPDATE FIELDS ( status )
       WITH VALUE #( ( %tky-trguid = pa_guid
                       status      = pa_stat ) )
       FAILED gs_failed.

 IF gs_failed IS NOT INITIAL.
   MESSAGE e102 WITH pa_guid.
 ELSE.

   COMMIT ENTITIES.

   WRITE: / 'Status of instance', pa_guid,
            'successfully set to', pa_stat.
 ENDIF.
