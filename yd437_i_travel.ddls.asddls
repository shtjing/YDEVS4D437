@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'YD437_I_TRAVEL'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity YD437_I_TRAVEL as select from yd437_travel
{
  key trguid    as Trguid,
     agencynum  as AgencyID,
     travelid   as TravelID,
     trdesc     as TravelDescription,
     customid   as CustomerID,
     stdat      as StartDate,
     enddat     as EndDate,
     status     as Status,
     @Semantics.systemDateTime.lastChangedAt: true
     changed_at as ChangedAt,
     changed_by as ChangedBy
//      _association_name // Make association public   
}
