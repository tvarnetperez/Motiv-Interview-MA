

# _____ _____ _____ 
#| __  |     |  _  |
#| __ -|  |  |   __|
#|_____|_____|__|   
  
  
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
# paper 8: "Full-mouth clinical examinations were before treatment (baseline) and 3 and 12 months after the non-surgical treatment and oral hygiene intervention" p.3
# paper 12: "14 weeks after baseline (12 weeks after non-surgical therapy completion)" p.2

# column_labels <- c(
#                "Paper", "Authors",
#                "Control Baseline Mean", "Control Baseline SD",
#                "Control ~12w Mean", "Control ~12w SD",
#                "Treatment Baseline Mean", "Treatment Baseline SD",
#                "Treatment ~12w Mean", "Treatment ~12w SD",
#                "Control Size", "Treatment Size"
#                "Control Size", "Treatment Size"
#                "Control pre-post correlation", "Treatment pre-post correlation"
#                )


#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# _1.1 Full Mouth #####

mouth_8 <- list('p8', "Jönsson et al., 2010",
  75L, 18L, # Control mean and SD
  33L, 15L, # Follow-up 12-weeks
  70L, 20L, # Treatment mean and SD
  24L, 12L, # Follow-up
  56L, 57L, # Sample sizes Control-Treatment
  56L, 57L,  # Follow-up   # Paper 8 just has sample size for 52 week follow-up, but values were imputed so will use same value
  ri_temp, ri_temp
) # p.5

mouth_12 <- list('p12',"Donos et al., 2020", 
  0.4, 0.18, # Control
  0.2, 0.13,
  0.5, 0.21, # Treatment
  0.2, 0.11,
  34L, 37L,
  31L, 33L,
  ri_temp, ri_temp
) 

# "As previously indicated, 37 Test and 34 Control patients were enrolled at the screening visit, while two Control patients dropped out at baseline and
# 1 at week 2 (so did not attend Visit 5). Among the Test patients, two dropped out at week 2 (so did not attend Visit 5) and 2 at week 8 (so did not
# attend Visit 6)." p.6

mouth_12 <- c(mouth_12[1:2], unlist(mouth_12[3:10]) * 100L, mouth_12[11:16]) # We will put in the same percentage scale

mouth_all <- list(mouth_8, mouth_12)
df_mouth <- do.call(rbind.data.frame, mouth_all)

colnames(df_mouth) <- cols_short

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 2. Effect Size Calculation ----------------------------------------------


#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# _2.1 Full Mouth ------------------------------------------------------------


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
  ni = t_n2, # Paper 8 just has sample size for 52 week follow-up, but values were imputed so will use same value
  ri = t_ri,
  slab = authors
)


# Define dataset with difference in mean changes among
# treatment arms
eff_data_mouth <- data.frame(yi = mouth_t_mc$yi - mouth_c_mc$yi,
                             vi = mouth_t_mc$vi + mouth_c_mc$vi)


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

# As it's two studies, we will consider the result of the Q-statistic underpowered a priori.

# Random-Effects Model (k = 2; tau^2 estimator: REML)
# 
# tau^2 (estimated amount of total heterogeneity): 5.772 (SE = 25.456)
# tau (square root of estimated tau^2 value):      2.402
# I^2 (total heterogeneity / total variability):   32.07%
# H^2 (total variability / sampling variability):  1.47
# 
# Test for Heterogeneity:
#   Q(df = 1) = 1.472, p-val = 0.225
# 
# Model Results:
#   
#   estimate     se    zval   pval    ci.lb   ci.ub 
# -6.410  2.941  -2.179  0.029  -12.175  -0.645  * 
#   
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Fixed Effects
mouth_FE <- metafor::rma.uni(
  yi, vi,
  data = eff_data_mouth,
  method = "FE",
  digits = 3
)  


# 4. Plot --------------------------------------------------------------------



# Random Effects

#pdf(file = "RE_BOP.pdf", width = 8.27, height = 4.13)

df_obj <- df_mouth
rma_obj <- mouth_RE
model_type <- "Random Effects"
response_type <- "Bleeding On Probing (in %)"

metafor::forest(rma_obj,
                ilab = cbind(df_obj$c_0_mean, df_obj$t_0_mean,
                             df_obj$c_n2, df_obj$t_n2),
                ilab.xpos = seq(-55, -25, length.out = 4),
                xlim = c(-100, 40), # Plot section position
                cex = 1, # Character size
                header = c("Study", "Mean Change [95% CI]"), 
                efac = 2, top = 2,
                mlab = "")

op <- par(cex = 0.80, font = 2)
text(seq(-55, -25, length.out = 4), 3.5, c("Cont.", "Treat."))
text(seq(-50, -30, length.out = 2), 4, c("Baseline Mean", "Sample Size"))
par(op)
text(-100, -1, cex = 0.80, paste0("Response: ", response_type , "\nPre-post correlation: ",
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
response_type <- "Bleeding On Probing (in %)"

# dev.new(width = 8.27, height = 4.13, noRStudioGD = TRUE)

metafor::forest(rma_obj,
                ilab = cbind(df_obj$c_0_mean, df_obj$t_0_mean,
                             df_obj$c_n2, df_obj$t_n2),
                ilab.xpos = seq(-55, -25, length.out = 4),
                xlim = c(-100, 40), # Plot section position
                cex = 1, # Character size
                header = c("Study", "Mean Change [95% CI]"), 
                efac = 2, top = 2,
                mlab = "")

op <- par(cex = 0.80, font = 2)
text(seq(-55, -25, length.out = 4), 3.5, c("Cont.", "Treat."))
text(seq(-50, -30, length.out = 2), 4, c("Baseline Mean", "Sample Size"))
par(op)
text(-100, -1, cex = 0.80, paste0("Response: ", response_type , "\nPre-post correlation: ",
                                   formatC(df_obj$c_ri, digits = 2, format = "f"), " (cont.), ",
                                   formatC(df_obj$t_ri, digits = 2, format = "f"), " (treat.).",
                                   paste0("\n", model_type, " Model."), 
                                   " I² :", formatC(rma_obj$I2, digits = 1, format = "f"),
                                   "%."#τ² : ",  formatC(rma_obj$tau2, digits = 1, format = "f"),
                                   #"\nEstimator: ",  formatC(rma_obj$method, format = "s")), pos = 4
), pos = 4)

