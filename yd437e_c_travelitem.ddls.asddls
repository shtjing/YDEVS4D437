@EndUserText.label: 'projection YD437e_C_TravelItem'
@AccessControl.authorizationCheck: #NOT_REQUIRED

@Metadata.allowExtensions: true

define view entity YD437e_C_TravelItem as projection on YD437e_I_TravelItem
{
key Itguid,
     AgencyID,
     TravelID,
     ItemID,
     Trguid,
 
     @Consumption.valueHelpDefinition: 
              [ { entity: { name:    'YD437_I_Carrier',
                            element: 'CarrierID'
                          }
                }
              ]
     CarrierID,
     
     @Consumption.valueHelpDefinition: 
              [ { entity: { name:    'YD437_I_Connection',
                            element: 'ConnectionID'
                          },
                  additionalBinding: 
                       [ { localElement: 'CarrierID',
                                element: 'CarrierID',
                                  usage: #FILTER_AND_RESULT
                         }
                       ],
                  label: 'Value Help by Connection' 
                },
                { entity: { name:    'YD437_I_FlightVH',
                            element: 'ConnectionID'
                          },
                  additionalBinding: 
                       [ { localElement: 'CarrierID',
                           element:      'CarrierID',
                           usage:        #FILTER_AND_RESULT
                         },
                         { localElement: 'FlightDate',
                           element:      'FlightDate',
                           usage:         #RESULT
                        }
                      ],
                  label: 'Value Help by Flight',
                  qualifier: 'Secondary Value help'
                }
              ]
     ConnectionID,
     @Consumption.valueHelpDefinition: 
              [ { entity: { name:    'YD437_I_FlightVH',
                            element: 'FlightDate'
                          },
                  additionalBinding: 
                       [ { localElement: 'CarrierID',
                           element:      'CarrierID',
                           usage:         #FILTER_AND_RESULT
                         },
                         { localElement: 'ConnectionID',
                           element:      'ConnectionID',
                           usage:        #RESULT
                         }
                       ]
                }
              ]
     FlightDate,
     BookingID,
     @Consumption.valueHelpDefinition: 
              [ { entity: { name:    'YD437_I_FlightClassVH',
                            element: 'FlightClass'
                          }
                }
              ]
 
     FlightClass,
     PassengerName,
     CreatedAt,
     CreatedBy,
     ChangedAt,
     ChangedBy,
     LocalChangedAt,
 
     _Travel : redirected to parent YD437e_C_Travel
}
