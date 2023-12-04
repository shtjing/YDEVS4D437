@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Flight Class Text Provider'
 
@VDM.viewType: #BASIC
 
@ObjectModel.dataCategory: #TEXT
@ObjectModel.representativeKey: 'FlightClass'
 
 
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity YD437_I_FlightClassT 
  as select from dd07t
 association[1] to YD437_I_FlightClass as _FlightClass
 on $projection.FlightClass = _FlightClass.FlightClass    
   {  
       @ObjectModel.foreignKey.association: '_FlightClass'
   key domvalue_l as FlightClass,
 
       @Semantics.language: true
   key ddlanguage as Language,
 
       @Semantics.text: true
       ddtext     as Text,
       
       _FlightClass
 }
   
 where domname  = 'S_CLASS'
   and as4local = 'A' 
