*&---------------------------------------------------------------------*
*& Report YD437B_EML_S2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT YD437B_EML_S2 MESSAGE-ID Zdevs4d437.

PARAMETERS pa_guid TYPE Y_trguid DEFAULT '00000000000000000000000000000000'.
PARAMETERS pa_stat TYPE Y_status VALUE CHECK.

* Data declarations for read access

DATA gt_read_import   TYPE TABLE     FOR READ IMPORT YD437_I_TRAVEL.
DATA gs_read_import   TYPE STRUCTURE FOR READ IMPORT YD437_I_TRAVEL.

DATA gt_read_result   TYPE TABLE     FOR READ RESULT YD437_I_TRAVEL.
DATA gs_read_result   TYPE STRUCTURE FOR READ RESULT YD437_I_TRAVEL.

* Data declarations for update access

DATA gt_update_import TYPE TABLE     FOR UPDATE      YD437_I_TRAVEL.
DATA gs_update_import TYPE STRUCTURE FOR UPDATE      YD437_I_TRAVEL.

* Data declarations for response

DATA gs_failed        TYPE RESPONSE  FOR FAILED      YD437_I_TRAVEL.
DATA gs_reported      TYPE RESPONSE  FOR REPORTED    YD437_I_TRAVEL.

START-OF-SELECTION.

* Read the RAP BO entity to check for current status
*---------------------------------------------------*
 CLEAR gt_read_import.

 gs_read_import-%tky-trguid = pa_guid.  "Recommended: use named include %tky
 "gs_read_import-trguid      = pa_guid. " Alternative: directly access trguid

 APPEND gs_read_import TO gt_read_import.

*  READ ENTITY YD437_I_TRAVEL
*  ALL FIELDS WITH gt_read_import
*  RESULT   gt_read_result
*  FAILED   gs_failed.

 READ ENTITIES OF YD437_I_TRAVEL
   ENTITY YD437_I_TRAVEL
   ALL FIELDS WITH gt_read_import
   RESULT   gt_read_result
   FAILED   gs_failed.

 IF gs_failed IS NOT INITIAL.

   MESSAGE e103 WITH pa_guid.

 ELSE.

   READ TABLE gt_read_result INTO gs_read_result INDEX 1.

   IF gs_read_result-status = pa_stat.
     MESSAGE e110 WITH pa_guid pa_stat.
   ENDIF.

 ENDIF.

* Update RAP BO with new status
*-------------------------------*

 CLEAR gt_update_import.

 gs_update_import-%tky-trguid = pa_guid.
 gs_update_import-status = pa_stat.

 APPEND gs_update_import TO gt_update_import.

*  MODIFY ENTITY YD437_I_TRAVEL
*        UPDATE FIELDS ( status ) WITH gt_update_import
*        FAILED gs_failed.

 MODIFY ENTITIES OF YD437_I_TRAVEL
      ENTITY YD437_I_TRAVEL
        UPDATE FIELDS ( status ) WITH gt_update_import
        FAILED gs_failed.


 IF gs_failed IS NOT INITIAL.
   ROLLBACK ENTITIES.
   MESSAGE e102 WITH pa_guid.
 ELSE.

   COMMIT ENTITIES.

   WRITE: / 'Status of instance', pa_guid, 'successfully set to', pa_stat.
 ENDIF..
