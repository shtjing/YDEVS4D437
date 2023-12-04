@EndUserText.label: 'D437e_C_Travel'
@AccessControl.authorizationCheck: #CHECK

@Search.searchable: true

@Metadata.allowExtensions: true

define root view entity YD437e_C_Travel as projection on YD437e_I_Travel
{
 key Trguid,
     @Search.defaultSearchElement: true
     AgencyID,  
     TravelID,
     TravelDescription,
 
     @Consumption.valueHelpDefinition: [{
                      entity: {
                                name:    'YD437_I_Customer',
                                element: 'Customer'
                              }
                                       }]
 
     @Search.defaultSearchElement: true
     CustomerID,
     StartDate,
     EndDate,
     Status,
     ChangedAt,
     ChangedBy,
 
     LocalChangedAt,
     
     _TravelItem: redirected to composition child YD437e_C_TravelItem
}
