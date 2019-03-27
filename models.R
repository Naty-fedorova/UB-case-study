# regress house_invest on .

d <- s$hh_df

# recode out zeros in house investment
d$H0 <- ifelse( d$house_invest==0 , 1L , 0L )

# basic model with only random effects

dat <- list(
	H0 = d$H0,
	house_invest = d$house_invest,
	fam_id = d$fam_id,
	capital = d$capital,
	intend_stay = d$intend_stay
)

m0 <- ulam(
	alist(
		H0 ~ bernoulli( pH ),
		logit(pH) <- ah + bC0*capital + bIS0*intend_stay,
		ah ~ normal(0,1),

		house_invest|H0==1 ~ normal( muH , sigmaH ),
		muH <- a + bC*capital + bIS*intend_stay,

		c(a,bC,bIS,bC0,bIS0) ~ normal(0,1),

		sigmaH ~ exponential(1)
	),
	data = dat , sample=TRUE , control=list(max_treedepth=15) )

m1 <- ulam(
	alist(
		H0 ~ bernoulli( pH ),
		logit(pH) <- ah + v[fam_id,1] + bC0*capital + bIS0*intend_stay,
		ah ~ normal(0,1),

		house_invest|H0==1 ~ normal( muH , sigmaH ),
		muH <- a + v[fam_id,2] + bC*capital + bIS*intend_stay,

		c(a,bC,bIS,bC0,bIS0) ~ normal(0,1),

		matrix[20,2]:v <- compose_noncentered( sigma_fam , L_Rho_fam , z ),
		matrix[2,20]:z ~ normal(0,1),
		cholesky_factor_corr[2]:L_Rho_fam ~ lkj_cholesky_corr(2),
		vector[2]:sigma_fam ~ exponential(1),

		sigmaH ~ exponential(1)
	),
	data = dat , sample=TRUE , control=list(max_treedepth=15) )
