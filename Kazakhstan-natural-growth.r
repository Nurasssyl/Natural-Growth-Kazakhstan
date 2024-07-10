library(devtools)
library(geokz)
library(dplyr)
library(sf)
library(tmap)

devtools::install_github("arodionoff/geokz")

file_path <- 'C:/Users/Admin/Рабочий стол/Audandar-natural-growth.csv'
Natural_Growth_df <- read.csv(file_path, sep = ";", fileEncoding = "ISO-8859-1")

cat("First few rows of Natural Growth DataFrame:\n")
print(head(Natural_Growth_df))

cat("Structure of Natural Growth DataFrame:\n")
str(Natural_Growth_df)

Natural_Growth_df <- Natural_Growth_df %>%
  dplyr::select(ADM2_PCODE, difference) %>%
  dplyr::rename(ISO_3166_2 = ADM2_PCODE)

Natural_Growth_df$ISO_3166_2 <- trimws(as.character(Natural_Growth_df$ISO_3166_2))
Natural_Growth_df$difference <- as.numeric(Natural_Growth_df$difference)

cat("Number of NAs in 'difference' column after conversion:\n")
print(sum(is.na(Natural_Growth_df$difference)))

cat("Unique values in ISO_3166_2 of Natural Growth DataFrame:\n")
print(unique(Natural_Growth_df$ISO_3166_2))

rayons_map <- get_kaz_rayons_map(Year = 2024)
rayons_map$ADM2_PCODE <- trimws(as.character(rayons_map$ADM2_PCODE))

cat("Unique values in ADM2_PCODE of rayons_map:\n")
print(unique(rayons_map$ADM2_PCODE))

map_data <- dplyr::left_join(
  x = rayons_map,
  y = Natural_Growth_df,
  by = c("ADM2_PCODE" = "ISO_3166_2")
)

cat("First few rows of joined map_data:\n")
print(head(map_data))

cat("Number of NAs in 'difference' column of map_data after join:\n")
print(sum(is.na(map_data$difference)))

# Fill NA values in difference column with the mean of the non-NA differences
mean_difference <- mean(map_data$difference, na.rm = TRUE)
map_data$difference[is.na(map_data$difference)] <- mean_difference

map_data$difference_color <- ifelse(map_data$difference > 0, "green", 
                                    ifelse(map_data$difference < 0, "red", "yellow"))

cat("Color assignment based on difference:\n")
print(table(map_data$difference_color))

map_plot <- tmap::tm_shape(map_data) +
  tmap::tm_fill("difference_color", title = "Difference") +
  tmap::tm_borders()

tmap::tmap_save(map_plot, "C:/Users/Admin/Рабочий стол/Audandar_Natural_Growth.jpeg")
