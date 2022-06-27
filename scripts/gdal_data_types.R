# Data frame used to match terra data types to gdal data types

data_types <- tibble(terra = c("LOG1s", 
                               "INT1S", "INT1U", 
                               "INT2S", "INT2U", 
                               "INT4S", "INT4U", 
                               "FLT4S", "FLT8S"), 
                     gdal = c(NA, 
                              NA, "Byte",
                              "Int16", "UInt16", 
                              "Int32", "UInt32", 
                              "Float32", "Float64"))
