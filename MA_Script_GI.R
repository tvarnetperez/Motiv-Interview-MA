# ______  _____ 
#| | ____  | |  
#| |  | |  | |  
#|_|__|_| _|_|_ 
#

# Goes from 0 to 3
#The LOE & SILNESS gingival index (GI), as described by
# LOE (30), was used to assess gingival inflammation. The
# mesial, buccal, distal, and lingual/palatal sites of each
# tooth present were scored for GI as follows: normal gingiva,
# score = 0; slight change in color or slight inflammation
# without bleeding on probing indicating mild
# inflammation, score = 1; signs of redness and edematous
# gingiva with bleeding on probing showing moderate
# inflammation, score = 2; or remarkable redness and edema
# with susceptibility to spontaneous bleeding demonstrating
# severe inflammation, score = 3.  (Paper 2, Saffari et al., 2020, p.3)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 1. Data Input -----------------------------------------------------------

cols_short <- c(
  "paper", "authors",
  "c_0_mean", "c_0_sd",
  "c_12_mean", "c_12_sd",
  "t_0_mean", "t_0_sd",
  "t_12_mean", "t_12_sd",
  "c_n1", "t_n1",
  "c_n2", "t_n2",
  "c_ri", "t_ri"
)


# 12 and 14 weeks are equivalent times
# paper 2: "Intervention and control groups were followed for 3 months after the intervention at which point outcomes were assessed." p.3
# paper 12: "14 weeks after baseline (12 weeks after non-surgical therapy completion)" p.2


#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# 1.1 Full Mouth #####

mouth_2 <- list(
  'p2', "Saffari et al., 2020",
  0.9, 0.4, # Control mean and SD
  1.6, 0.5, # Follow-up
  1.1, 0.7, # Treatment mean and SD
  0.5, 0.7, # Follow-up
  56L, 56L, # Sample sizes Control, Treatment
  50L, 52L, # Follow-up
  ri_temp, ri_temp # Pre-post correlation
) 

mouth_12 <- list(
  'p12',"Donos et al., 2020", 
  0.6, 0.34, # Control mean and SD
  0.3, 0.19, # Follow-up
  0.7, 0.31, # Treatment mean and SD
  0.3, 0.15, # Follow-up
  34L, 37L, # Sample sizes Control, Treatment
  31L, 33L, # Follow-up
  ri_temp, ri_temp
) 

mouth_all <- list(mouth_2, mouth_12)
df_mouth <- do.call(rbind.data.frame, mouth_all)

colnames(df_mouth) <- cols_short


#     # Estimation of change score SD from confidence interval for paper 2 (See 7.7.3.2. Cochrane handbook)
#       # Using t distribution as there is no information and sample size is under 60
#       # Maybe CI is derived from transformed values or maybe approximation error (CI non symmetrical around the mean)
#       # Double square brackets choose the value of just the first row in the dataframe, i.e. paper 2
# 
# sd_c_p2 <- (sqrt(df_mouth$c_n2)[[1]] * (1.2 - 0.3)) / (2*qt(0.975, df_mouth$c_n2[[1]] - 1))
# sd_t_p2 <- (sqrt(df_mouth$t_n2)[[1]] * (-0.2 - -1.1)) / (2*qt(0.975, df_mouth$t_n2[[1]] - 1))
# # Very large values
# 
# 
#     # Pre-post correlation as function of standard deviations
# ri_c_p2 <- (df_mouth$c_0_sd[[1]]^2 + df_mouth$c_12_sd[[1]]^2 - sd_c_p2^2) /
#   (2 * df_mouth$c_0_sd[[1]] * df_mouth$c_12_sd[[1]]) # Out of bounds
# 
# ri_t_p2 <- (df_mouth$t_0_sd[[1]]^2 + df_mouth$t_12_sd[[1]]^2 - sd_t_p2^2) /
#   (2 * df_mouth$t_0_sd[[1]] * df_mouth$t_12_sd[[1]]) # Out of bounds

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 2. Effect Size Calculation ----------------------------------------------

#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# 2.1 Full Mouth ------------------------------------------------------------

# Control
mouth_c_mc <- metafor::escalc(
  measure = "MC", 
  data = df_mouth,
  m1i = c_12_mean, m2i = c_0_mean, # metafor computes m1i - m2i, so post goes first.
  sd1i = c_12_sd, sd2i = c_0_sd,
  ni = c_n2, 
  ri = c_ri,
  slab = authors
)



# Treatment
mouth_t_mc <- metafor::escalc(
  measure = "MC", 
  data = df_mouth,
  m1i = t_12_mean, m2i = t_0_mean, # metafor computes m1i - m2i, so post goes first.
  sd1i = t_12_sd, sd2i = t_0_sd,
  ni = t_n2, 
  ri = t_ri,
  slab = authors
)


# Define dataset with difference in mean changes among
# treatment arms
eff_data_mouth <- data.frame(yi = mouth_t_mc$yi - mouth_c_mc$yi,
                             vi = mouth_t_mc$vi + mouth_c_mc$vi)


#   # ANCOVA Effect Size estimate (Kebede et al., 2018, p. 61, McKenzie et al., 2015, p. 374)
# 
# ancova_c_p2 <- 
# ancova_t_p2 <- 





#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 3. Model fitting --------------------------------------------------------


#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# 3.2 Full Mouth ------------------------------------------------------------

# Random effects
mouth_RE <- metafor::rma.uni(
  yi, vi,
  data = eff_data_mouth,
  method = "REML",
  digits = 3
)  

# Random-Effects Model (k = 2; tau^2 estimator: REML)
# 
# tau^2 (estimated amount of total heterogeneity): 0.712 (SE = 1.018)
# tau (square root of estimated tau^2 value):      0.844
# I^2 (total heterogeneity / total variability):   98.92%
# H^2 (total variability / sampling variability):  92.47
# 
# Test for Heterogeneity:
#   Q(df = 1) = 92.469, p-val < .001
# 
# Model Results:
#   
#   estimate     se    zval   pval   ci.lb  ci.ub 
# -0.697  0.600  -1.162  0.245  -1.873  0.479    
# 



# Fixed effects
mouth_FE <- metafor::rma.uni(
  yi, vi,
  data = eff_data_mouth,
  method = "FE",
  digits = 3
)



# 4. Plot -----------------------------------------------------------------

#pdf(file = "RE_GI.pdf", width = 8.27, height = 4.13)

df_obj <- df_mouth
rma_obj <- mouth_RE
model_type <- "Random Effects"
response_type <- "Gingival Index, from 0 (healthiest) to 3"

#dev.new(width = 8.27, height = 4.13, noRStudioGD = TRUE)

metafor::forest(rma_obj,
                ilab = cbind(df_obj$c_0_mean, df_obj$t_0_mean,
                             df_obj$c_n2, df_obj$t_n2),
                ilab.xpos = seq(-5.4, -2.6, length.out = 4),
                xlim = c(-8, 3), # Plot section position
                cex = 1, # Character size
                header = c("Study", "Mean Change [95% CI]"), 
                efac = 2, top = 2,
                mlab = "")

op <- par(cex = 0.80, font = 2)
text(seq(-5.4, -2.6, length.out = 4), 3.5, c("Cont.", "Treat."))
text(seq(-4.9, -3.1, length.out = 2), 4, c("Baseline Mean", "Sample Size"))
par(op)
text(-8, -1, cex = 0.80, paste0("Response: ", response_type , "\nPre-post correlation: ",
                                  formatC(df_obj$c_ri, digits = 2, format = "f"), " (cont.), ",
                                  formatC(df_obj$t_ri, digits = 2, format = "f"), " (treat.).",
                                  paste0("\n", model_type, " Model."), 
                                  " I² :", formatC(rma_obj$I2, digits = 1, format = "f"),
                                  "%, τ² : ",  formatC(rma_obj$tau2, digits = 1, format = "f"),
                                  "\nEstimator: ",  formatC(rma_obj$method, format = "s")), pos = 4)

#dev.off()


# Fixed Effects



df_obj <- df_mouth
rma_obj <- mouth_FE
model_type <- "Fixed Effects"
response_type <- "Gingival Index, from 0 (healthiest) to 3"

# dev.new(width = 8.27, height = 4.13, noRStudioGD = TRUE)

metafor::forest(rma_obj,
                ilab = cbind(df_obj$c_0_mean, df_obj$t_0_mean,
                             df_obj$c_n2, df_obj$t_n2),
                ilab.xpos = seq(-5.4, -2.6, length.out = 4),
                xlim = c(-8, 3), # Plot section position
                cex = 1, # Character size
                header = c("Study", "Mean Change [95% CI]"), 
                efac = 2, top = 2,
                mlab = "")

op <- par(cex = 0.80, font = 2)
text(seq(-5.4, -2.6, length.out = 4), 3.5, c("Cont.", "Treat."))
text(seq(-4.9, -3.1, length.out = 2), 4, c("Baseline Mean", "Sample Size"))
par(op)
text(-8, -1, cex = 0.80, paste0("Response: ", response_type , "\nPre-post correlation: ",
                                formatC(df_obj$c_ri, digits = 2, format = "f"), " (cont.), ",
                                formatC(df_obj$t_ri, digits = 2, format = "f"), " (treat.).",
                                paste0("\n", model_type, " Model."), 
                                " I² :", formatC(rma_obj$I2, digits = 1, format = "f"),
                                "%."#τ² : ",  formatC(rma_obj$tau2, digits = 1, format = "f"),
                                #"\nEstimator: ",  formatC(rma_obj$method, format = "s")), pos = 4
), pos = 4)