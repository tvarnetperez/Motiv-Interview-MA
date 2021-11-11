# 0. Dependencies ---------------------------------------------------------

library(metafor)

# 1. Plaque Index ---------------------------------------------------------

  # Placeholder/imputed pre-post correlation
ri_temp <- 0.59

eval(parse("MA_Script_PI.R",
           encoding = "UTF-8"))

# 2. Bleeding On Probing --------------------------------------------------

  # Placeholder/imputed pre-post correlation
ri_temp <- 0.59

eval(parse("MA_Script_BOP.R",
           encoding = "UTF-8"))



# 3. Gingival Index ---------------------------------as----------------------

  # Placeholder/imputed pre-post correlation
ri_temp <- 0.59

eval(parse("MA_Script_GI.R",
           encoding = "UTF-8"))

