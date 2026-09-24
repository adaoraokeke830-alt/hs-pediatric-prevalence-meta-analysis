library(meta)

# =========================
# MAIN DEPRESSION ANALYSIS
# =========================

depression_data <- data.frame(
  study = c(
    "Wright 2022",
    "Tiri 2018",
    "Seivright 2021",
    "Hallock 2021",
    "Dienes 2026",
    "Numani 2025"
  ),
  events = c(136, 13, 14, 59, 5, 41),
  total = c(1162, 153, 73, 1094, 39, 182)
)

depression_meta <- metaprop(
  event = events,
  n = total,
  studlab = study,
  data = depression_data,
  sm = "PLOGIT",
  method = "Inverse",
  method.tau = "REML",
  random = TRUE,
  common = FALSE
)

summary(depression_meta)

# =====================================
# SENSITIVITY ANALYSIS 1
# REMOVE DIENES 2026
# =====================================

depression_data_sensitivity1 <- data.frame(
  study = c(
    "Wright 2022",
    "Tiri 2018",
    "Seivright 2021",
    "Hallock 2021",
    "Numani 2025"
  ),
  events = c(136, 13, 14, 59, 41),
  total = c(1162, 153, 73, 1094, 182)
)

depression_meta_sensitivity1 <- metaprop(
  event = events,
  n = total,
  studlab = study,
  data = depression_data_sensitivity1,
  sm = "PLOGIT",
  method = "Inverse",
  method.tau = "REML",
  random = TRUE,
  common = FALSE
)

summary(depression_meta_sensitivity1)

# =====================================
# SENSITIVITY ANALYSIS 2
# REMOVE NUMANI 2025
# =====================================

depression_data_sensitivity2 <- data.frame(
  study = c(
    "Wright 2022",
    "Tiri 2018",
    "Seivright 2021",
    "Hallock 2021",
    "Dienes 2026"
  ),
  events = c(136, 13, 14, 59, 5),
  total = c(1162, 153, 73, 1094, 39)
)

depression_meta_sensitivity2 <- metaprop(
  event = events,
  n = total,
  studlab = study,
  data = depression_data_sensitivity2,
  sm = "PLOGIT",
  method = "Inverse",
  method.tau = "REML",
  random = TRUE,
  common = FALSE
)

summary(depression_meta_sensitivity2)

)
# =========================
# ANXIETY META-ANALYSIS
# =========================

anxiety_data <- data.frame(
  study = c(
    "Tiri 2018",
    "Hallock 2021",
    "Seivright 2021",
    "Dienes 2026",
    "Numani 2025"
  ),
  events = c(9, 367, 16, 8, 62),
  total = c(153, 1094, 73, 39, 182)
)

anxiety_meta <- metaprop(
  event = events,
  n = total,
  studlab = study,
  data = anxiety_data,
  sm = "PLOGIT",
  method = "Inverse",
  method.tau = "REML",
  random = TRUE,
  common = FALSE
)

summary(anxiety_meta)

)
# =====================================
# ANXIETY SENSITIVITY ANALYSIS
# REMOVE TIRI 2018
# =====================================

anxiety_data_sensitivity1 <- data.frame(
  study = c(
    "Hallock 2021",
    "Seivright 2021",
    "Dienes 2026",
    "Numani 2025"
  ),
  events = c(367, 16, 8, 62),
  total = c(1094, 73, 39, 182)
)

anxiety_meta_sensitivity1 <- metaprop(
  event = events,
  n = total,
  studlab = study,
  data = anxiety_data_sensitivity1,
  sm = "PLOGIT",
  method = "Inverse",
  method.tau = "REML",
  random = TRUE,
  common = FALSE
)

summary(anxiety_meta_sensitivity1)
