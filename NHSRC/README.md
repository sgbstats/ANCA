## Materials for the NHS R Community Conference

The work for this study was still ongoing at the time of the conference and so results may change to final publication if there are any data updates from centres. The data cleaning scripts cannot be published due to confidentiality.

# Abstract: Validating time-to-event clinical prediction models: A case study in ANCA-associated glomerulonephritis.

**Sebastian** Bate and Silke R Brix on behalf of the ANCA PRISMA group (**P**redictive **Ri**sk **S**tratification in ANCA-Associated Glomerulonephritis **Ma**nagement) 

**Background**: External validation and recalibration of clinical prediction models, especially with time-to-event endpoints, has been challenging due to a lack of transparency in reporting. Brix, *et al.*, published the *ANCA Renal Risk Score* in 2018 which uses estimated glomerular filtration rate (eGFR), percentage of normal glomeruli, and severity of tubular atrophy and interstitial fibrosis to predict end-stage kidney disease in ANCA-associated glomerulonephritis. 

**Methods**:  Validation and recalibration in a large, retrospective, international cohort of over 1400 patients was led by Manchester University NHS FT and the University of Manchester. *Harrell’s C* and novel methods for calibration using *{survival}* with an extension of logistic calibration plots to right-censored data were used to assess model fit. *Shiny* was used to create a clinician-friendly tool for predicting ESKD-free survival estimates. 

**Results**: The original score performed adequately (C=0.798) but had calibration issues. In reassessment, eGFR was dropped from the score in favour of serum creatinine. Two similar models were chosen to fulfill different clinical needs. The ‘continuous parameters’ model (C=0.823), which underpins the Shiny app, was used to calculate direct estimates of survival, and the ‘discrete categories’ model (C=0.820) is proposed as a basis for a risk stratification gold standard in future prospective studies.

**Conclusion**: A greater push is required to encourage researchers to publish their baseline hazard, coefficients, and underlying data as standard. We leveraged R to create quality and transparency as standard, with a focus on clear results, which can be easily replicated by third parties.
