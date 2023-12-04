@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Flight Travel Item'
define view entity YD437e_I_TravelItem
 as select from yd437e_tritem
 association to parent YD437e_I_Travel as _Travel 
          on $projection.Trguid = _Travel.Trguid
 
{
 key itguid         as Itguid,
     agencynum      as AgencyID,
     travelid       as TravelID,
     tritemno       as ItemID,
     trguid         as Trguid,
     carrid         as CarrierID,
     connid         as ConnectionID,
     fldate         as FlightDate,
     bookid         as BookingID,
     class          as FlightClass,
     passname       as PassengerName,
     @Semantics.systemDateTime.createdAt: true
     created_at     as CreatedAt,
     @Semantics.user.createdBy: true
     created_by     as CreatedBy,
     @Semantics.systemDateTime.lastChangedAt: true
     changed_at     as ChangedAt,
     @Semantics.user.lastChangedBy: true
     changed_by     as ChangedBy,
     @Semantics.systemDateTime.localInstanceLastChangedAt: true
     loc_changed_at as LocalChangedAt,
     
     _Travel
}
