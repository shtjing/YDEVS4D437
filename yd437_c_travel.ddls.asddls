@EndUserText.label: 'Projection YD437_I_TRAVEL'
@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
define root view entity YD437_C_TRAVEL as projection on YD437_I_TRAVEL
{
    key Trguid,
    AgencyID,
    TravelID,
    TravelDescription,
    CustomerID,
    StartDate,
    EndDate,
    Status,
    ChangedAt,
    ChangedBy
}
