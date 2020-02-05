# Row differences
library(manymodelr)
testthat::test_that("Test rowdiff",
                    code={
  
                      
  dummy_data <- data.frame(A=c(1,1,2,3), B=c(4,5,3,7))
  
  the_test <- rowdiff(dummy_data,
                      direction="reverse")[3,2]
  testthat::expect_equal(the_test, -2)
  
  another_test <- rowdiff(dummy_data,
                          direction="forward")[3,2]
  testthat::expect_equal(another_test, -4)
  
 # Replace NAs frrom calculation
testthat::expect_equal(rowdiff(dummy_data, na.rm=TRUE,
          na_action = "value", value="Self diff")[4,1],
          "Self diff", fixed=TRUE)
          
  
                    })
