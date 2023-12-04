@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Flight Class'
 
@VDM.viewType: #BASIC
 
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity  YD437_I_FlightClass
 as select from dd07l
 association [0..*] to Yd437_I_FlightClassT as _description 
 on $projection.FlightClass = _description.FlightClass
 
{
     @ObjectModel.text.association: '_description'
 key domvalue_l as FlightClass,
 
     _description
}
 
where
     domname  = 'S_CLASS'
 and as4local = 'A'
