#Deploy app

library(rsconnect)

rsconnect::setAccountInfo(name='xcesspoint',
                          token='E3763E5FCE049C8E9E546E159C7E19E2',
                          secret='6SzZZuEpjL8kyu0ZIy9/cz87F1Sj7Sg4loJzEeT9')

rsconnect::deployApp('C:/IS415/Project/geospatial_analytics/app')
