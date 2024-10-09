############
## Task 5 ##
############

library(tidyr)
library(ggplot2)
library(dplyr)
library(corrplot)

setwd("/home/ozkanali/Downloads")

ek2023 <- read.csv("ek2023.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "Windows-1252")
tulot2017 <- read.csv("tulot2017.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "Windows-1252")

colnames(ek2023) <- trimws(tolower(colnames(ek2023)))
colnames(tulot2017) <- trimws(tolower(colnames(tulot2017)))
ek2023$alue <- trimws(tolower(ek2023$alue))
tulot2017$alue <- trimws(tolower(tulot2017$alue))

merged_data <- merge(ek2023, tulot2017, by = "alue")

numeric_columns <- c("tulot", "mediaanitulot", "ansiotulot", "sdp", "ps", "kok", "kesk", "vihr", "vas")
merged_data[numeric_columns] <- lapply(merged_data[numeric_columns], as.numeric)

correlation_matrix <- cor(merged_data[, numeric_columns], use = "complete.obs")
print(correlation_matrix)
corrplot::corrplot(correlation_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 45)

ggplot(merged_data, aes(x = tulot, y = kesk)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Support for KESK vs Average Income", x = "Average Income", y = "Support for KESK (%)")

ggplot(merged_data, aes(x = tulot, y = vihr)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Support for VIHR vs Average Income", x = "Average Income", y = "Support for VIHR (%)")

model_kesk <- lm(kesk ~ tulot + mediaanitulot + ansiotulot, data = merged_data)
summary(model_kesk)

model_vihr <- lm(vihr ~ tulot + mediaanitulot + ansiotulot, data = merged_data)
summary(model_vihr)

hypothetical_data <- data.frame(tulot = 30000, mediaanitulot = 25000, ansiotulot = 28000)

predicted_kesk <- predict(model_kesk, newdata = hypothetical_data)
cat("Predicted support for KESK: ", predicted_kesk, "%\n")

predicted_vihr <- predict(model_vihr, newdata = hypothetical_data)
cat("Predicted support for VIHR: ", predicted_vihr, "%\n")
