@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Flight Carrier'
define view entity YD437_I_Carrier
 as select from scarr
{
 key carrid   as CarrierID,
     carrname as CarrierName,
     currcode as CurrencyCode,
     url      as WebPage
}
