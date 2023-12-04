@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Flight Connection'
 
@VDM.viewType: #BASIC
 
define view entity YD437_I_Connection  
        as select from spfli  
           association[0..1] to YD437_I_Carrier as _Carrier
           on $projection.CarrierID = _Carrier.CarrierID 
{
     @ObjectModel.foreignKey.association: '_Carrier'
 key carrid                                 as CarrierID, 
 key connid                                 as ConnectionID,
 
     fltype                                 as FlightType,
 
     airpfrom                               as OriginAirport,    
     cityfrom                               as OriginCity,
     countryfr                              as OriginCountry,
 
     airpto                                 as DestinationAirport,
     cityto                                 as DestinationCity,
     countryto                              as DestinationCoutry,
 
     @Semantics.quantity.unitOfMeasure: 'UnitForDistance'
     distance                               as FlightDistance,
     distid                                 as UnitForDistance,
 
     deptime                                as DepartureTime,
     arrtime                                as ArrivalTime,
 
     period                                 as ArrivalDaysLater,
//      fltime                                 as FlightDuration,
    
     _Carrier
 
}
