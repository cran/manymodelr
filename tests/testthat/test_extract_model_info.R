test_that(desc= "extract_model_info works as expected",
                    code= {
                      skip_on_oldrel()
            lm_model <- fit_model(yields, "height","weight","lm")
            aov_model <- fit_model(yields, "height","weight","aov")
            # Still need to figure out the best way to expect an equal value
            # say p values, r2, etc.
            # for now:
          expect_error(extract_model_info(lm_model), 
                       "model_object and what are both required", fixed= TRUE)
          expect_error(extract_model_info(aov_model), 
                       "model_object and what are both required", fixed= TRUE)
          expect_equal(nrow(extract_model_info(aov_model,"msq")), 2)
          expect_error(extract_model_info(aov_model,
                                          "nope",
        "what should be one of coeffs df ssq msq f_value p_value resids aic predictors response interactions residuals",
          fixed=TRUE))
          expect_equal(extract_model_info(aov_model,"predictors"), 
                       "weight")
          expect_type(extract_model_info(lm_model,"resids"),"double")
          expect_type(extract_model_info(lm_model,"residuals"),"double")
          expect_equal(length(extract_model_info(aov_model,c("ssq","msq"))),2)
          # lme models
           # this is purely demonstrative
          fm2 <- lmer(weight ~ height + (1 | normal), data=yields)
          expect_equal(extract_model_info(fm2,"random_groups")[[1]],2)
          
          expect_error(extract_model_info(fm2),
                       "model_object and what are both required",
                       fixed = TRUE)
          expect_error(extract_model_info(fm2,"gibberish"),
        "what should be one of fixed_effects resids log_lik random_groups random_effects reml formula coefficients residuals",
                       fixed=TRUE)
          expect_equal(length(extract_model_info(fm2, c("reml","log_lik"))),2)
          
              
                    })