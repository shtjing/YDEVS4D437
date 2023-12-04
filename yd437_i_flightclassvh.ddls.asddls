@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Flight Class Value Help'
 
@VDM.viewType: #COMPOSITE
 
@ObjectModel.dataCategory: #VALUE_HELP
@ObjectModel : { resultSet.sizeCategory: #XS }
 
define view entity YD437_I_FlightClassVH 
 as select from YD437_I_FlightClass
 association [*] to YD437_I_FlightClassT as _description 
 on $projection.FlightClass = _description.FlightClass
 
     {
         @Consumption.filter.hidden: true      
         @ObjectModel.text.association: '_description'
     key FlightClass,
   
         _description
   }
