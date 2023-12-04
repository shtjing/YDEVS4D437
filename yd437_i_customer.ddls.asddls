@AbapCatalog.sqlViewName: 'YD437_ICUSTOMER'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Yd437_i_customer'
define view Yd437_i_customer as select from scustom
{
 key id                               as Customer,
     name                             as Name,
     form                             as Form,
     concat_with_space(form, name, 1) as FullName,
     street                           as Street,
     postbox                          as PostBox,
     postcode                         as PostCode,
     city                             as City,
     country                          as Country,
     region                           as Region,
     telephone                        as Telephone,
     custtype                         as CustomerType,
     discount                         as Discount,
     langu                            as Language,
     email                            as Email   
}
