@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help for Flight'
 
@VDM.viewType: #COMPOSITE
 
@ObjectModel.dataCategory: #VALUE_HELP
 
define view entity YD437_I_FlightVH 
   as select from YD437_I_Flight 
{
    @UI.selectionField: [{position: 50 }]
key CarrierID,
   @Consumption.filter.hidden: true
key ConnectionID,
    @UI.selectionField: [{position: 30 }]
key FlightDate,
    @UI.selectionField: [{position: 10 }]
    _Connection.OriginCity,
    @UI.selectionField: [{position: 40 }]
   _Connection.DepartureTime,
 
    @UI.selectionField: [{position: 20 }]
    _Connection.DestinationCity,
    @UI.selectionField: [{position: 50 }]
   _Connection.ArrivalTime,
    @Consumption.filter.hidden: true
    _Connection.ArrivalDaysLater,
 
    @Consumption.filter.hidden: true
    _Carrier,
    
    @Consumption.filter.hidden: true
    _Connection
 
}
