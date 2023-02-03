library(ggplot2)
library(patchwork)
library(data.table)
library(IndexWizard)
sessionInfo()
# R version 4.2.1 (2022-06-23 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8    LC_MONETARY=German_Germany.utf8
# [4] LC_NUMERIC=C                    LC_TIME=German_Germany.utf8    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] IndexWizard_0.1.3.2 data.table_1.14.6   patchwork_1.1.2     ggplot2_3.4.0      
# 
# loaded via a namespace (and not attached):
#   [1] pillar_1.8.1     compiler_4.2.1   tools_4.2.1      digest_0.6.30    evaluate_0.19    lifecycle_1.0.3  tibble_3.1.8    
# [8] gtable_0.3.1     pkgconfig_2.0.3  rlang_1.0.6      cli_3.4.1        rstudioapi_0.14  yaml_2.3.6       xfun_0.35       
# [15] fastmap_1.1.0    withr_2.5.0      dplyr_1.0.10     knitr_1.41       generics_0.1.3   vctrs_0.5.0      grid_4.2.1      
# [22] tidyselect_1.2.0 glue_1.6.2       R6_2.5.1         fansi_1.0.3      rmarkdown_2.19   farver_2.1.1     magrittr_2.0.3  
# [29] scales_1.2.1     htmltools_0.5.3  colorspace_2.0-3 labeling_0.4.2   utf8_1.2.2       munsell_0.5.0    crayon_1.5.2  

# set whether to save plots ----------------------------------------------------

savePlots <- FALSE
if(savePlots && !dir.exists("plots_publication")){
  dir.create("plots_publication")
}

# data -------------------------------------------------------------------------
tn <- c("RZM", "RZN", "RZEo", "RZEn", "RZR", "RZKm", "RZKd", "RZH", "RZC", "RZS")

w_old <- c(0.45, 0.2, 0.15, 0, 0.1, 0.03, 0, 0, 0, 0.07)
names(w_old) <- tn; w_old

w_new <- c(0.36, 0.18, 0, 0.15, 0.07, 0.015, 0.015, 0.18, 0.03, 0)
names(w_new) <- tn; w_old

tno <- tn[w_old!=0]
tnn <- tn[w_new!=0]

G <- matrix(
  c(1.0,0.13,0.13,0.07,-0.15,0.11,0.07,0.09,-0.02,0.04,
    0.13,1.0,0.23,0.28,0.43,0.25,0.22,0.78,0.13,0.46,
    0.13,0.23,1.0,0.92,0.02,0.09,-0.05,0.25,-0.1,0.19,
    0.07,0.28,0.92,1.0,0.06,0.08,-.03,0.31,-.1,0.25,
    -0.15,0.43,.02,0.06,1.0,0.32,0.19,0.41,0.04,0.15,
    0.11,0.25,0.09,0.08,0.32,1.0,0.0,0.25,0.04,0.13,
    0.07,0.22,-.05,-.03,0.19,0,1.0,0.23,0.05,0.10,
    0.09,0.78,0.25,0.31,0.41,0.25,0.23,1.0,0.1,0.57,
    -.02,0.13,-.10,-.1,0.04,0.04,0.05,0.1,1.0,0.02,
    0.04,0.46,0.19,0.25,0.15,0.13,0.10,0.57,0.02,1.0)
  ,byrow = TRUE, nrow = length(tn), ncol = length(tn), dimnames = list(tn, tn)
)
G <- G*144

r2 <- c(0.743, 0.673, 0.638, 0.717, 0.541, 0.635, 0.604, 0.720, 0.499, 0.764)
names(r2) <- tn
r2_old <- r2[w_old != 0]
r2_new <- r2[w_new != 0]

deltautr <- c(0.28401392, 0.21637703, 0.17932963, 0.09986764, 0.08767239, 0.13273939)
names(deltautr) <- tn[w_old>0]


h2 <- c(0.314, 0.090, 0.194, 0.194, 0.013, 0.049, 0.033, 0.061, 0.014, 0.273)
names(h2) <- tn

H <- matrix (
  c(1.00,0.06,0.06,-.20,0.05,0.03,
    0.06,1.00,0.14,0.40,0.20,0.46,
    0.06,0.14,1.00,-.03,0.03,0.15,
    -.20,0.40,-.03,1.00,0.30,0.13,
    0.05,0.20,0.03,0.30,1.00,0.11,
    0.03,0.46,0.15,0.13,0.11,1.00),
  nrow=6, ncol=6,
  dimnames = list(tn[w_old != 0], tn[w_old != 0]))
H <- H * 144

# set gplobal parameters -------------------------------------------------------
col6 <- c('dodgerblue3','firebrick3','cyan4','lightgoldenrod4','coral2','deeppink3') # colors for 6 old traits
col8 <- c('dodgerblue3','firebrick3','cyan2','lightgoldenrod4','coral2','cornflowerblue','darkviolet', 'olivedrab3') # colors for 8 new traits

# Figure 1 - expected composition of trend -------------------------------------


res_uncorr <- SelInd(w = w_old[tno],  G = G[tno,tno],  r2 = r2_old, verbose = FALSE)
res_corr <- SelInd(w = w_old[tno],  G = G[tno,tno],  r2 = r2_old, H = H, verbose = FALSE)

dat <- data.table(
  traits = tno,
  `E uncorrelated` = res_uncorr$d_G_exp_scaled[tno],
  `E correlated` = res_corr$d_G_exp_scaled[tno],
  `observed` = deltautr[tno]
)
dat_long <- melt(dat,id.vars = "traits")
dat_long[,traits:=factor(traits,levels = tno)]

col2 <- matrix(c(col6,col6,rep('white',length(col6))),ncol=3)
col2 <- c(t(col2))

pl <- ggplot(dat_long,
             aes(
               x = traits,
               y = value,
               fill = interaction(variable,traits),
               color = traits,
               alpha = variable,
               size = variable,
               group = interaction(traits, variable)
             )) +
  geom_bar(position = 'dodge',
           stat = 'identity') +
  scale_fill_manual(values = col2) +
  scale_color_manual(values = col6) +
  scale_size_manual(values = c(0.5,0.5,0.5))+ # change line size
  scale_alpha_manual(values = c(1, 0.5, 0.5)) + # potentially change alpha level of middle bar
  theme_minimal(base_size = 12) + # change base size of text in pt
  theme(legend.position = c(0.99,0.99),
        legend.justification = c(1,1),
        legend.background = element_rect(fill = 'white',color = 'grey')) +
  labs(x = element_blank(),
       y = 'proportion', # change y axis label!
       alpha = element_blank()) + # change legend name!
  guides(fill = guide_none(),
         color = guide_none(),
         size = guide_none(),
         alpha = guide_legend(override.aes = list(fill = c('black','black','white'),
                                                  color = 'black')))
pl

# save plot as svg
if(savePlots){
  svg(
    'plots_publication/Fig1.svg', # file path
    width = 17/cm(1), # plot width in cm transformed to inches
    height = 8/cm(1) # plot height
  )
  pl
  dev.off()
  # save plot as tiff
  tiff(
    'plots_publication/Fig1.tiff', # file path
    width = 17, # plot width in cm
    height = 8, # plot height
    res = 600,
    compression = "lzw",
    units = "cm"
  )
  pl
  dev.off()
}

# figure 2 - measures of importance --------------------------------------------


res <- SelInd(w = w_old[tno],  G = G[tno,tno],  r2 = r2_old, H = H, verbose = FALSE)
dat <- data.table(
  traits = tno,
  r_IP = res$r_IP[tno],
  r_IH = res$r_IH[tno]*-1
)
dat[,traits:=factor(traits,levels = tno)]

{
  pl1 <- ggplot(dat,
                aes(x = traits,
                    y = r_IP,
                    fill = traits))+
    geom_bar(stat = 'identity')+
    scale_fill_manual(values = col6)+
    theme_minimal()+
    theme(plot.tag.position = c(0.95,0.9))+
    labs(x = element_blank(),
         y = expression(r[Iy[i]]),
         tag = "A")+
    guides(fill = guide_none())
  pl2 <- ggplot(dat,
                aes(x = traits,
                    y = r_IH,
                    fill = traits))+
    geom_bar(stat = 'identity')+
    scale_fill_manual(values = col6)+
    scale_x_discrete(labels = element_blank())+
    theme_minimal()+
    theme(plot.tag.position = c(0.95,0.8),)+
    labs(x = element_blank(),
         y = expression(r[IH[-j]]/r[IH]),
         tag = 'B')+
    guides(fill = guide_none())
  pl2$theme$plot.margin[1] <- unit(0,"points")
  pl1$theme$plot.margin[3] <- unit(0,"points")
  
  pl1 / pl2
  
}

if(savePlots){
  svg(
    'plots_publication/Fig2.svg', # file path
    width = 17/cm(1), # plot width in cm transformed to inches
    height = 10/cm(1) # plot height
  )
  pl1 / pl2
  dev.off()
  # save plot as tiff
  tiff(
    'plots_publication/Fig2.tiff', # file path
    width = 17, # plot width in cm
    height = 10, # plot height
    res = 600,
    compression = "lzw",
    units = "cm"
  )
  pl1 / pl2
  dev.off()
}

# figure 3 - expected composition of phenotypic progress --------------------------------------------


res <- SelInd(w = w_old[tno],  G = G[tno,tno],  r2 = r2_old, H = H, h2 = h2[tno], verbose = FALSE)
dat <- data.table(
  traits = tno,
  d_P_exp_scaled = res$d_P_exp_scaled[tno]
)
dat[,traits:=factor(traits,levels = tno)]

pl1 <- ggplot(dat,
              aes(x = traits,
                  y = d_P_exp_scaled,
                  fill = traits))+
  geom_bar(stat = 'identity')+
  scale_fill_manual(values = col6)+
  theme_minimal()+
  labs(x = element_blank(),
       y = "relative phenotypic progress")+
  guides(fill = guide_none())

if(savePlots){
  svg(
    'plots_publication/Fig3.svg', # file path
    width = 17/cm(1), # plot width in cm transformed to inches
    height = 8/cm(1) # plot height
  )
  pl1
  dev.off()
  # save plot as tiff
  tiff(
    'plots_publication/Fig3.tiff', # file path
    width = 17, # plot width in cm
    height = 8, # plot height
    res = 600,
    compression = "lzw",
    units = "cm"
  )
  pl1
  dev.off()
}

# figure 4 - predefined vs. effective economic weights -------------------------

res <- SelInd(w = w_old[tno],  G = G[tno,tno],  r2 = r2_old, H = H, d_G_obs = deltautr, verbose = FALSE)

dat <- data.table(
  traits = tno,
  predefined = res$w[tno],
  effective = res$w_real[tno]
)
dat_long <- melt(dat,id.vars = "traits")
dat_long[,traits:=factor(traits,levels = tno)]
dat_long[,variable:=factor(variable,levels = c("predefined","effective"))]

pl <- ggplot(dat_long,
             aes(
               x = traits,
               y = value,
               fill = traits,
               #color = traits,
               alpha = variable,
               group = interaction(traits, variable)
             )) +
  geom_bar(position = 'dodge',
           stat = 'identity') +
  scale_fill_manual(values = col6) +
  #scale_color_manual(values = col6) +
  scale_alpha_manual(values = c(1, 0.5)) + # potentially change alpha level of middle bar
  theme_minimal(base_size = 12) + # change base size of text in pt
  theme(legend.position = c(0.99,0.99),
        legend.justification = c(1,1),
        legend.background = element_rect(fill = 'white',color = 'grey')) +
  labs(x = element_blank(),
       y = 'economic weights', # change y axis label!
       alpha = element_blank()) + # change legend name!
  guides(fill = guide_none(),
         color = guide_none(),
         size = guide_none(),
         alpha = guide_legend(override.aes = list(fill = c('black','black'),
                                                  color = NA)))
pl

if(savePlots){
  # save plot as svg
  svg(
    'plots_publication/Fig4.svg', # file path
    width = 17/cm(1), # plot width in cm transformed to inches
    height = 8/cm(1) # plot height
  )
  pl
  dev.off()
  # save plot as tiff
  tiff(
    'plots_publication/Fig4.tiff', # file path
    width = 17, # plot width in cm
    height = 8, # plot height
    res = 600,
    compression = "lzw",
    units = "cm"
  )
  pl
  dev.off()
}

# figure 5 - composition of genetic trend old vs new -------------------------

res_old <- SelInd(w = w_old,  G = G,  r2 = r2_old, verbose = FALSE)
res_new <- SelInd(w = w_new,  G = G,  r2 = r2_new, verbose = FALSE)

dat <- data.table(
  traits = tnn,
  `old index` = res_old$d_G_exp_scaled[tnn],
  `new index` = res_new$d_G_exp_scaled[tnn]
)
dat[,`old index` := `old index`/sum(abs(`old index`))]
dat[,`new index` := `new index`/sum(abs(`new index`))]

dat_long <- melt(dat,id.vars = "traits")
dat_long[,traits:=factor(traits,levels = tnn)]
dat_long[,variable:=factor(variable,levels = c("old index","new index"))]

pl <- ggplot(dat_long,
             aes(
               x = traits,
               y = value,
               fill = traits,
               #color = traits,
               alpha = variable,
               group = interaction(traits, variable)
             )) +
  geom_bar(position = 'dodge',
           stat = 'identity') +
  scale_fill_manual(values = col8) +
  #scale_color_manual(values = col8) +
  scale_alpha_manual(values = c(1, 0.5)) + # potentially change alpha level of middle bar
  theme_minimal(base_size = 12) + # change base size of text in pt
  theme(legend.position = 'bottom') +
  labs(x = element_blank(),
       y = 'realtive genetic gain', # change y axis label!
       alpha = element_blank()) + # change legend name!
  guides(fill = guide_none(),
         color = guide_none(),
         size = guide_none(),
         alpha = guide_legend(override.aes = list(fill = c('black','black'),
                                                  color = NA)))
pl

if(savePlots){
  # save plot as svg
  svg(
    'plots_publication/Fig5.svg', # file path
    width = 17/cm(1), # plot width in cm transformed to inches
    height = 8/cm(1) # plot height
  )
  pl
  dev.off()
  # save plot as tiff
  tiff(
    'plots_publication/Fig5.tiff', # file path
    width = 17, # plot width in cm
    height = 8, # plot height
    res = 600,
    compression = "lzw",
    units = "cm"
  )
  pl
  dev.off()
}
# table 3 ----------------------------------------------------------------------
# calculations require subsetting of w and G
res_old <- SelInd(
  w = w_old[w_old != 0],
  G = G[names(w_old[w_old != 0]),names(w_old[w_old != 0])],
  H = H,
  r2 = r2_old,
  verbose = FALSE)

# no H matrix available for new index traits -- assume residual covariance to be 0
res_new <- SelInd(
  w = w_new[w_new != 0],
  G = G[names(w_new[w_new != 0]),names(w_new[w_new != 0])],
  r2 = r2_new,
  verbose = FALSE)

## table 3A - mit H --------------------------------------------------------------------
round(res_old$del_d_scaled, 2)
round(res_old$del_d_scaled_new_diff, 4) # absolute difference in gain per trait
round(res_old$del_d_scaled_new, 2) # scaled so that sum of abs(rows) = 1
round(res_old$del_d_scaled_new_pook, 2) # pook scaling


## table 3A - ohne H --------------------------------------------------------------------
res_old <- SelInd(
  w = w_old[w_old != 0],
  G = G[names(w_old[w_old != 0]),names(w_old[w_old != 0])],
  r2 = r2_old,
  verbose = FALSE)
round(res_old$del_d_scaled, 2)
round(res_old$del_d_scaled_new_diff, 4) # absolute difference in gain per trait
round(res_old$del_d_scaled_new, 2) # scaled so that sum of abs(rows) = 1
round(res_old$del_d_scaled_new_pook, 2) # pook scaling

## table 3B --------------------------------------------------------------------
round(res_new$del_d_scaled, 2) # old results
round(res_new$del_d_scaled_new_diff, 4) # absolute difference in gain per trait
round(res_new$del_d_scaled_new, 2) # scaled so that sum of abs(rows) = 1
rowSums(abs(res_new$del_d_scaled_new))

# Comment Pook:
# Ich wuerde vorschlagen zu skalieren das die Diagonale 1 ist um auszudruecken wie viel
# sich andere Merkmale veraendern pro zusaetzlichem Zuchtfortschritt um einen Plus im
# Merkmal mit dem hoeheren Index
round(res_new$del_d_scaled_new_pook, 2) # pook scaling

res_new$del_d_scaled_new_diff %*% res_new$w
res_new$del_d_scaled_new %*% res_new$w
res_new$del_d_scaled_new_pook %*% res_new$w
