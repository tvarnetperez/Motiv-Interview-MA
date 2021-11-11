
#   ______  _____ 
#  | |  | \  | |  
#  | |__|_/  | |  
#  |_|      _|_|_ 

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 1. Data Input -----------------------------------------------------------

cols_short <- c(
  "paper", "authors",
  "c_0_mean", "c_0_sd",
  "c_4_mean", "c_4_sd",
  "c_12_mean", "c_12_sd",
  "t_0_mean", "t_0_sd",
  "t_4_mean", "t_4_sd",
  "t_12_mean", "t_12_sd",
  "c_n1", "t_n1",
  "c_n2", "t_n2",
  "c_ri", "t_ri"
)

#               c(
#                "Paper", "Authors",
#                "Control Baseline Mean", "Control Baseline SD",
#                "Control 4w Mean", "Control 4w SD", 
#                "Control ~12w Mean", "Control ~12w SD",
#                "Treatment Baseline Mean", "Treatment Baseline SD",
#                "Treatment 4w Mean", "Treatment 4w SD", 
#                "Treatment ~12w Mean", "Treatment ~12w SD",
#                "Control Size", "Treatment Size",
#                "Control Post Size", "Treatment Post Size",
#                "Control pre-post correlation", "Treatment pre-post correlation"
#                )


    # Input data from Figure 2 in Paper 12 by 
    # using WebPlotDigitizer from automeris.io

control_p12 <- as.data.frame(read.table('../Figure 2 paper 12/BluePointsClean.csv', 
                                        sep = ";", header = FALSE, dec = ",", 
                                        col.names = c("X1", "X2")))

treatment_p12 <- as.data.frame(read.table('../Figure 2 paper 12/OrangePointsClean.csv', 
                                          sep = ";",  header = FALSE, dec = ",",
                                          col.names = c("X1", "X2")))

# Pre-allocate a data frame with correct types
df_p12 <- data.frame(
  paper = character(), authors = character(),
  c_0_mean = double(), c_0_sd = double(),
  c_4_mean = double(), c_4_sd = double(),
  c_12_mean = double() , c_12_sd = double(),
  t_0_mean = double(), t_0_sd = double(),
  t_4_mean = double(), t_4_sd = double(),
  t_12_mean = double() , t_12_sd = double(),
  c_n1 = integer(), t_n1 = integer(),
  c_n2 = integer(), t_n2 = integer(),
  c_ri = double(), t_ri = double()
)

# Will follow recommendations from Cochrane Handbook (7.7.3.2) to go from 
# confidence interval of a mean to its standard deviation
# using a t distribution, given sample size and lack of disclosure
# (An L following a number denotes an integer in the R language)

df_p12[1,] <- NA
df_p12$paper <-  'p12'
df_p12$authors <-  "Donos et al., 2020"
df_p12$c_0_mean <- control_p12[control_p12$X1 == "Bar24", "X2"][[1]]
df_p12$c_12_mean <- control_p12[control_p12$X1 == "Bar6", "X2"][[1]]
df_p12$c_0_sd <- sqrt(34L) * (control_p12[control_p12$X1 == "Bar23", "X2"][[1]] -
                                control_p12[control_p12$X1 == "Bar26", "X2"][[1]]) / (2 * qt(0.975, 33L))
df_p12$c_12_sd <- sqrt(31L) * (control_p12[control_p12$X1 == "Bar8", "X2"][[1]] -
                                 control_p12[control_p12$X1 == "Bar9", "X2"][[1]]) / (2 * qt(0.975, 30L))
df_p12$t_0_mean <- treatment_p12[treatment_p12$X1 == "Bar26", "X2"][[1]]
df_p12$t_12_mean <- treatment_p12[treatment_p12$X1 == "Bar4", "X2"][[1]]
df_p12$t_0_sd <-  sqrt(37L) * (treatment_p12[treatment_p12$X1 == "Bar27", "X2"][[1]] -
                                 treatment_p12[treatment_p12$X1 == "Bar28", "X2"][[1]]) / (2 * qt(0.975, 36L))
df_p12$t_12_sd <- sqrt(33L) * (treatment_p12[treatment_p12$X1 == "Bar5", "X2"][[1]] -
                                 treatment_p12[treatment_p12$X1 == "Bar6", "X2"][[1]]) / (2 * qt(0.975, 32L))
df_p12$c_n1 <- 34L
df_p12$c_n2 <- 31L
df_p12$t_n1 <- 37L
df_p12$t_n2 <- 33L

df_p12$c_ri <- ri_temp
df_p12$t_ri <- ri_temp

df_p12[,c("c_0_mean", "c_0_sd", "c_12_mean", "c_12_sd",
          "t_0_mean", "t_0_sd", "t_12_mean", "t_12_sd")] <- 
  lapply(df_p12[,c("c_0_mean", "c_0_sd", "c_12_mean", "c_12_sd",
                   "t_0_mean", "t_0_sd", "t_12_mean", "t_12_sd")], function(x) round(x, 1))

# Printed results:
# paper            authors c_0_mean c_0_sd c_4_mean c_4_sd c_12_mean c_12_sd t_0_mean t_0_sd t_4_mean t_4_sd t_12_mean
# 1   p12 Donos et al., 2020     65.2   21.1       NA     NA      48.6      18     63.7   25.1       NA     NA      44.6
# t_12_sd c_n1 t_n1 c_n2 t_n2 c_ri t_ri
# 1    23.7   34   37   31   33 0.59 0.59

#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# _1.1 Full Mouth #####

mouth_8 <- list(
  'p8', "Jönsson et al., 2010",
  57L, 17L, # Control Mean & SD
  NA, NA,     # 4w follow-up
  28L, 17L,   # 12w follow-up
  59L, 18L, # Treatment Mean & SD
  NA, NA,     # 4w follow-up
  17L, 10L,   # 12w follow-up
  56L, 57L, # Sample Size C & T
  56L, 57L,   # 12w follow-up
  ri_temp, ri_temp
  ) # See table 1 p.913, there are 5 subjects loss to follow-up but at the 12 month (52w). 
  # Also, linear interpolation imputation was used for the 4 subjects loss to the experimental group and 1 for the control group.
  # Standard deviation bars do not correspond to values in Fig. 1 and Fig. 2

mouth_12 <- unclass(df_p12[1,])
  

mouth_38 <- list(
  'p38',"Stenman et al., 2012",
  43.1, 19.2, # Control Mean & SD
  26.2, 17.1,   # 4w follow-up
  19.0, 13.3,   # 12w follow-up
  50.2, 21.5, # Treatment Mean & SD
  28.4, 16.5,   # 4w follow-up
  27.1, 15.2,   # 12w follow-up
  22L, 22L,   # Sample Size C & T
# 22L, 20L,     # Week 4
  20L, 19L,     # Week 12
  ri_temp, ri_temp
  )

mouth_39 <- list(
  'p39',"Godard, Dufour & Jeanne, 2011",
  58L, 12L, # Control Mean & SD
  54L, 12L,   # 4w  follow-up
  NA, NA,     # 12w follow-up
  55L, 15L, # Treatment Mean & SD
  34L, 20L,   # 4w follow-up
  NA, NA,     # 12w follow-up
  24L, 27L, # Sample Size C & T
  24L, 27L,   # 4w follow-up    # 2 were lost in the Control group and 5 in the experimental group. Values were imputed by LOCF, so there's effect attenuation bias
  ri_temp, ri_temp
  )

mouth_all <- list(mouth_8, mouth_12, mouth_38, mouth_39)
df_mouth <- do.call(rbind.data.frame, mouth_all)

colnames(df_mouth) <- cols_short


#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# _1.2 Proximal   ##############

# prox_8 <- list(
#   'p8', "Jönsson, Öhrn, Lindberg & Oscarson, 2010",
#   79L, 18L, # Control
#   NA, NA,
#   42L, 22L,
#   83L, 18L, # Treatment
#   NA, NA,
#   28L, 16L,
#   56L, 57L
# ) # p.5
# 
# prox_38 <- list(
#   'p38',"Stenman et al., 2012", # Week 4 sample sizes are 20 treatment, 22 control, Week 12 they're 19 and 20. Week 12 is post DH
#   59.9, 21.6, # Control
#   38.5, 24.8,
#   28.1, 19.2,
#   67.9, 25.2, # Treatment
#   41.7, 19.3,
#   38.6, 19.3,
#   22L, 20L) # n Week 4
# # 20L, 19L) # n Week 12
# 
# prox_39 <- list(
#   'p39',"Godard, Dufour & Jeanne, 2011",
#   68L, 0.23, # Control
#   73L, 0.27,
#   NA, NA,
#   65L, 0.22, # Treatment
#   45L, 0.30,
#   NA, NA,
#   27L, 24L
# )
# 
# prox_all <- list(prox_8, prox_38, prox_39)
# df_prox <- do.call(rbind.data.frame, prox_all)
# 
# colnames(df_prox) <- cols_short
# # names(df_prox) <- column_names

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 2. Effect Size Calculation ----------------------------------------------


#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# _2.1 Full Mouth ------------------------------------------------------------

# Will use paper 39 measurement at wave 4 of follow-up as it is the only measurement available


  # Control
df_mouth$c_post_mean <- c(df_mouth$c_12_mean[1:3], df_mouth$c_4_mean[4])
df_mouth$c_post_sd <-   c(df_mouth$c_12_sd[1:3], df_mouth$c_4_sd[4])

mouth_c_mc <- metafor::escalc(
  measure = "MC", # Raw Mean Change
  m1i = c_post_mean, m2i = c_0_mean, # metafor computes m1i - m2i, so post goes first.
  sd1i = c_post_sd, sd2i = c_0_sd,
  ri = c_ri,
  ni = c_n2,
  data = df_mouth,
  slab = authors
)

  # Treatment
df_mouth$t_post_mean <- c(df_mouth$t_12_mean[1:3], df_mouth$t_4_mean[4])
df_mouth$t_post_sd <-   c(df_mouth$t_12_sd[1:3], df_mouth$t_4_sd[4])

mouth_t_mc <- metafor::escalc(
  measure = "MC",
  m1i = t_post_mean, m2i = t_0_mean, # metafor computes m1i - m2i, so post goes first.
  sd1i = t_post_sd, sd2i = t_0_sd,
  ri = t_ri,
  ni = t_n2,
  data = df_mouth,
  slab = authors
)


# Define dataset with difference in mean changes among
# treatment arms
eff_data_mouth <- data.frame(yi = mouth_t_mc$yi - mouth_c_mc$yi,
                            vi = mouth_t_mc$vi + mouth_c_mc$vi)


#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# _2.2 Proximal ------------------------------------------------------------


# # Will use the third study post measurement at 4 weeks instead of 12
# # as it is its only measurement
# df_prox$post_c_mean <- c(df_prox$c_12_mean[1:2], df_prox$c_4_mean[3])
# df_prox$post_c_sd <- c(df_prox$c_12_sd[1:2], df_prox$c_4_sd[3])
# 
#   # Control
# prox_C_md <- metafor::escalc(
#   measure = "MD", # Due to same measurement scale
#     data = df_prox,
#   m1i = c_0_mean, m2i = post_c_mean,
#   sd1i = c_0_sd, sd2i = post_c_sd,
#   n1i = c_n, n2i = c_n # Paper 8 just has sample size for 52 week follow-up, which is 55-53,
#   # will approximate to the control sample size then
#   )
# 
# # Paper 39 refers to 2 (Control) and 5 (Treatment) subjects lost to follow-up
# # but reports they were still included in the analysis (LOCF)
# 
# 
# 
# df_prox$post_t_mean <- c(df_prox$t_12_mean[1:2], df_prox$t_4_mean[3])
# df_prox$post_t_sd <- c(df_prox$t_12_sd[1:2], df_prox$t_4_sd[3])
# 
#   # Treatment
# prox_T_md <- metafor::escalc(
#   measure = "MD", # Due to same measurement scale
#   data = df_prox,
#   m1i = t_0_mean, m2i = post_t_mean,
#   sd1i = t_0_sd, sd2i = post_t_sd,
#   n1i = t_n, n2i = t_n
# )
# 
# 
# # Define dataset with difference in mean changes among
# # treatment arms
# eff_data_prox <- data.frame(yi = prox_T_md$yi - prox_C_md$yi,
#                        vi = prox_T_md$vi + prox_C_md$vi)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 3. Model fitting --------------------------------------------------------


# _3.1 Full-mouth ----------------------------------------------------------

# Random Effects
mouth_RE <- metafor::rma.uni(
  yi, vi,
  data = eff_data_mouth,
  method = "REML",
  digits = 3
)  

# Random-Effects Model (k = 4; tau^2 estimator: REML)
# 
# tau^2 (estimated amount of total heterogeneity): 44.006 (SE = 50.959)
# tau (square root of estimated tau^2 value):      6.634
# I^2 (total heterogeneity / total variability):   71.80%
# H^2 (total variability / sampling variability):  3.55
# 
# Test for Heterogeneity:
#   Q(df = 3) = 12.535, p-val = 0.006
# 
# Model Results:
#   
#   estimate     se    zval   pval    ci.lb  ci.ub 
#     -3.541  3.949  -0.897  0.370  -11.280  4.199    
# 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

rstudent(mouth_RE)
# Paper 8  (Jönsson et al., 2010) significantly strays away from the rest:
#                               resid     se      z 
#Jönsson et al., 2010          -13.648  4.011 -3.402 


  #Fixed Effects
mouth_FE <- metafor::rma.uni(
  yi, vi,
  data = eff_data_mouth,
  method = "FE",
  digits = 3
)

# Fixed-Effects Model (k = 4)
# 
# I^2 (total heterogeneity / total variability):   76.07%
# H^2 (total variability / sampling variability):  4.18
# 
# Test for Heterogeneity:
#   Q(df = 3) = 12.535, p-val = 0.006
# 
# Model Results:
#   
#   estimate     se    zval   pval   ci.lb   ci.ub 
# -6.067  1.970  -3.080  0.002  -9.928  -2.206  ** 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# _3.2 Proximal ------------------------------------------------------------



#   # Fixed effects
# metafor::rma.uni(
#   yi, vi,
#   data = eff_data_prox,
#   method = "FE",
#   digits = 3
#   )  
# 
# # Fixed-Effects Model (k = 3)
# # 
# # I^2 (total heterogeneity / total variability):   85.32%
# # H^2 (total variability / sampling variability):  6.81
# # 
# # Test for Heterogeneity:
# #   Q(df = 2) = 13.627, p-val = 0.001
# # 
# # Model Results:
# #   
# #   estimate     se     zval   pval   ci.lb   ci.ub 
# # 24.991  0.102  244.854  <.001  24.791  25.191  *** 
# #   
# #   ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# 
# 
#   # Random effects
# metafor::rma.uni(
#   yi, vi,
#   data = eff_data_prox,
#   method = "REML",
#   digits = 3
# )  
# # 
# # Random-Effects Model (k = 3; tau^2 estimator: REML)
# # 
# # tau^2 (estimated amount of total heterogeneity): 141.745 (SE = 166.840)
# # tau (square root of estimated tau^2 value):      11.906
# # I^2 (total heterogeneity / total variability):   91.56%
# # H^2 (total variability / sampling variability):  11.85
# # 
# # Test for Heterogeneity:
# #   Q(df = 2) = 13.627, p-val = 0.001
# # 
# # Model Results:
# #   
# #   estimate     se   zval   pval  ci.lb   ci.ub 
# # 15.417  7.436  2.073  0.038  0.841  29.992  * 
# #   
# #   ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# #   




# 4. Plot --------------------------------------------------------------------



# _4.1 Forest Plot ---------------------------------------------------------


# Random Effects

  #pdf(file = "RE_PI.pdf", width = 8.27, height = 4.7)

#dev.new(width = 8.27, height = 4.7, noRStudioGD = TRUE)

df_obj <- df_mouth
rma_obj <- mouth_RE
model_type <- "Random Effects"
response_type <- "Plaque Index (in %)"

metafor::forest(rma_obj,
                ilab = cbind(df_obj$c_0_mean, df_obj$t_0_mean,
                             df_obj$c_n2, df_obj$t_n2),
                ilab.xpos = seq(-60, -30, length.out = 4),
                xlim = c(-120, 50), # Plot section position
                cex = 1, # Character size
                header = c("Study", "Mean Change [95% CI]"), 
                efac = 2, top = 2,
                mlab = "")

op <- par(cex = .80, font = 2)
text(seq(-60, -25, length.out = 4), 5.5, c("Cont.", "Treat."))
text(seq(-55, -30, length.out = 2), 6, c("Baseline Mean", "Sample Size"))
par(op)
text(-120, -1.2, cex = .80, paste0("Response: ", response_type , "\nPre-post correlation: ",
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
response_type <- "Plaque Index (in %)"

dev.new(width = 8.27, height = 4.7, noRStudioGD = TRUE)

metafor::forest(rma_obj,
                ilab = cbind(df_obj$c_0_mean, df_obj$t_0_mean,
                             df_obj$c_n2, df_obj$t_n2),
                ilab.xpos = seq(-60, -30, length.out = 4),
                xlim = c(-120, 50), # Plot section position
                cex = 1, # Character size
                header = c("Study", "Mean Change [95% CI]"), 
                efac = 2, top = 2,
                mlab = "")

op <- par(cex = .80, font = 2)
text(seq(-60, -25, length.out = 4), 5.5, c("Cont.", "Treat."))
text(seq(-55, -30, length.out = 2), 6, c("Baseline Mean", "Sample Size"))
par(op)
text(-120, -1.2, cex = .80, paste0("Response: ", response_type , "\nPre-post correlation: ",
                                   formatC(df_obj$c_ri, digits = 2, format = "f"), " (cont.), ",
                                   formatC(df_obj$t_ri, digits = 2, format = "f"), " (treat.).",
                                   paste0("\n", model_type, " Model."), 
                                   " I² :", formatC(rma_obj$I2, digits = 1, format = "f"),
                                   "%."#τ² : ",  formatC(rma_obj$tau2, digits = 1, format = "f"),
                                   #"\nEstimator: ",  formatC(rma_obj$method, format = "s")), pos = 4
                                   ), pos = 4)

# dev.off()

# _4.2 Funnel Plot ---------------------------------------------------------

metafor::funnel(mouth_RE)
metafor::funnel(metafor::trimfill(mouth_RE)) # Conclusion becomes a little bit stronger


# Estimated number of missing studies on the left side: 1 (SE = 1.578)
# 
# Random-Effects Model (k = 5; tau^2 estimator: REML)
# 
# tau^2 (estimated amount of total heterogeneity): 73.593 (SE = 66.286)
# tau (square root of estimated tau^2 value):      8.579
# I^2 (total heterogeneity / total variability):   80.36%
# H^2 (total variability / sampling variability):  5.09
# 
# Test for Heterogeneity:
#   Q(df = 4) = 16.385, p-val = 0.003
# 
# Model Results:
#   
#   estimate     se    zval   pval    ci.lb   ci.ub 
# -11.300  4.332  -2.608  0.009  -19.790  -2.809  ** 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# 5. Meta-regression ------------------------------------------------------

mouth_MR_RE <- metafor::rma.uni(
  yi, vi,
  data = eff_data_mouth,
  mods = I((df_mouth$c_0_mean + df_mouth$t_0_mean) / 2),
  method = "REML",
  digits = 3
)  # As expected, moderator is not relevant

# Mixed-Effects Model (k = 4; tau^2 estimator: REML)
# 
# logLik  deviance       AIC       BIC      AICc 
# -7.398    14.795    20.795    16.875    44.795   
# 
# tau^2 (estimated amount of residual heterogeneity):     75.671 (SE = 90.637)
# tau (square root of estimated tau^2 value):             8.699
# I^2 (residual heterogeneity / unaccounted variability): 84.38%
# H^2 (unaccounted variability / sampling variability):   6.40
# R^2 (amount of heterogeneity accounted for):            0.00%
# 
# Test for Residual Heterogeneity:
#   QE(df = 2) = 10.202, p-val = 0.006
# 
# Test of Moderators (coefficient 2):
#   QM(df = 1) = 0.162, p-val = 0.687
# 
# Model Results:
#   
#           estimate      se    zval   pval    ci.lb   ci.ub 
# intrcpt     9.660  45.170   0.214  0.831  -78.871  98.191    
# mods       -0.320   0.794  -0.403  0.687   -1.876   1.237    
# 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



