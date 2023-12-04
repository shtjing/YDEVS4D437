@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'DRAFT TABLE D437d_I_Travel'
define root view entity YD437d_I_Travel as select from yd437d_travel

//composition of target_data_source_name as _association_name
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
     loc_changed_at as LocalChangedAt   
//    _association_name // Make association public
}
