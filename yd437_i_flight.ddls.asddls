
   

@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Flight'
 
@VDM.viewType: #BASIC
 
define view entity YD437_I_Flight
 as select from sflight
  association[0..1] to YD437_I_Carrier    as _Carrier
  on $projection.CarrierID = _Carrier.CarrierID 
  association[0..1] to YD437_I_Connection as _Connection
  on $projection.CarrierID = _Connection.CarrierID
 and $projection.ConnectionID = _Connection.ConnectionID      
 
{
     @ObjectModel.foreignKey.association: '_Carrier'
 key carrid    as CarrierID,
     @ObjectModel.foreignKey.association: '_Connection'
 key connid    as ConnectionID,
 key fldate    as FlightDate,
     planetype as AirplaneType,
     @Semantics.amount.currencyCode: 'CurrencyCode'
     price     as FlightPrice,
     currency  as CurrencyCode,
     
     _Carrier,
     _Connection
     
}
