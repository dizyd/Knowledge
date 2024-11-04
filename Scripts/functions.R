
# Plotting                           ----------------------------------------------------------------

clrs <- c("#54AA8F","#00335B",
          "#22A884FF","#414487FF",
          "#496aa2","#e46c0a")



theme_nice <- function(){
  theme_minimal(base_family = "Jost") +  
    theme(plot.title       = element_text(hjust = 0.5,size = 20),
          panel.grid.minor = element_blank(),
          text             = element_text(size  = 20),
          panel.border     = element_rect(colour = "black", linewidth = 0.5, fill = NA),
          axis.title.x     = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
          axis.title.y     = element_text(margin = unit(c(3, 3, 0, 0), "mm"), angle = 90),
          legend.title     = element_text(face = "bold",size=16),
          strip.text       = element_text(face = "bold"),
          legend.position  = "bottom"
    )}

# Create anonymize function          -----------------------------------------------

anonymize <- Vectorize(function(name) {
  
  hash <- digest(name, algo = "sha256", serialize = FALSE)
  
  return(substr(hash, 1, 12))
  
}, vectorize.args = "name")


# LogNormal Stuff                    ---------------------------------------------------------------


# This function computes the mu and sigma parameters of
# a lognormal distribution with the desired M and SD values.

lognormal_mu_sigma_from_mean_sd <- function(M, SD, digits=2) {
  
  if (M <= 0) stop("M must be > 0")
  if (SD<= 0) stop("SD must be > 0")
  
  mu    <- log(M / sqrt(SD^2 / M^2 + 1))
  sigma <- sqrt(log(SD^2 / M^2 + 1))
  
  return(c("mu" = round(mu,digits), "sigma" = round(sigma,digits)))
}

lognormal_mu_sigma_from_mean_sd(5,1)


# This function computes the M and SD of a normal distribution 
# based on the mu and sigma parameters of the lognormal distribution.


mean_sd_from_lognormal_mu_sigma <- function(mu,sigma,digits=2){
  
  M  <- exp(mu + sigma^2/2)
  SD <- M * sqrt(exp(sigma^2)-1) 
  
  return(c("M" = round(M,digits),"SD" = round(SD,digits)))
  
}


#mean_sd_from_lognormal_mu_sigma(1.5898276, 0.1980422)


effect_from_lognormal_effect <- function(mu,sigma,effect,digits=2){
  
  M1 <- exp(mu+(sigma^2)/2)
  M2 <- exp(mu+effect+(sigma^2)/2)
  
  
  
  return("diff" = round(M2-M1,digits))
  
}


#effect_range_from_lognormal_effect(-1.03,0.75,-0.3)

# EZ printing                        ---------------------------------------------------------------


ez_tables <- function(var){
  
  
  t1 <- table(var) %>% as.data.frame()
  t2 <-prop.table(table(var)) %>% round(3) %>% as.data.frame()
  
  t1$prop_freq <- t2$Freq
  names(t1)    <- c("var","freq","prop_freq")
  
  return(t1)
  
}

