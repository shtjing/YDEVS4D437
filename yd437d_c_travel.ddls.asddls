@EndUserText.label: 'Projection YD437D_I_TRAVEL'
@AccessControl.authorizationCheck: #CHECK
@Search.searchable: true
@Metadata.allowExtensions: true


define root view entity YD437D_C_TRAVEL
  as projection on YD437d_I_Travel
{
  key Trguid,
      @Search.defaultSearchElement: true
      AgencyID,
      TravelID,
      TravelDescription,
      @Consumption.valueHelpDefinition: [{
      entity: { name:    'YD437_I_Customer',                                
                element: 'Customer'    }  }]        
      @Search.defaultSearchElement: true
      CustomerID,
      StartDate,
      EndDate,
      Status,
      ChangedAt,
      ChangedBy,
      LocalChangedAt
}
