# Write dimensions and utilities down for the different countries

prefixes <- c("SI", "IN", "MO", "RE", "DA", "PH", "FU")
range <- 0:3
dimensions <- unlist(lapply(prefixes, function (p) paste(p, range, sep = "_")))

# The Netherlands
Netherlands <- c(-0.211, -0.137, -0.007, 0,
                 -0.184, -0.118, -0.018, 0,
                 -0.311, -0.179, -0.063, 0,
                 -0.269, -0.172, -0.015, 0,
                 -0.213, -0.140, -0.021, 0,
                 -0.383, -0.243, -0.064, 0,
                 -0.170, -0.106, 0, 0)




# Overall
df_utilities_countries <- data.frame(dimensions, Netherlands)


# Save the dataset
usethis::use_data(df_utilities_countries, overwrite = TRUE)

## VOOR NU LATER WEGHALEN

saveRDS(object = df_utilities_countries, "data/df_utilities_countries.RDS")
