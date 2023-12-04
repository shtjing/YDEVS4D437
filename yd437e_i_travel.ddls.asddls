@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'D437e_I_Travel'

define root view entity YD437e_I_Travel as select from yd437e_travel
composition[0..*] of YD437e_I_TravelItem as _TravelItem
{
    
 key trguid     as Trguid,
     agencynum  as AgencyID,
     travelid   as TravelID,
     trdesc     as TravelDescription,
     customid   as CustomerID,
     stdat      as StartDate,
     enddat     as EndDate,
     status     as Status,
     @Semantics.systemDateTime.lastChangedAt: true
     changed_at as ChangedAt,
     @Semantics.user.lastChangedBy: true
     changed_by as ChangedBy,
     @Semantics.systemDateTime.localInstanceLastChangedAt: true
     loc_changed_at as LocalChangedAt,
     
    _TravelItem
}
