
# ------ Functions from GraphicalRepresentation ------

test_that("cleaning takes away strange values", {
  data("train")
  df <- train[1:5,]
  expect_equal(dim(cleaning(df)), c(5,8))
})

test_that("main is of the right size", {
  expect_equal(dim(main()), c(8930,1))
})

test_that("cleaning takes away strange values", {
  data("train")
  grid <- main()
  expect_equal(is.null(safely(over_2)(train[1,4], train[1,5], grid)[[2]]), TRUE)
})


# ---------- Functions from TaxiModel --------------

test_that("transformation names are correct",{
  grid <- main()
  data("train")
  expect_equal(names(transformation(train[1,],grid)),
               c("long1", "lat1", "long2", "lat2",
                 "case_depart", "case_arrivee",
                 "week_day", "hour", "price", "passenger" ))
  })

test_that("hour_filter returns a string",{
  expect_equal(hour_filter(12),"10-16")
})

test_that("path names are correct", {
  grid <- main()
  data("train")
  expect_equal(names(path(transformation(train[1,],grid))),
               c("final_df$IDs","final_df$Passagers",
                 "final_df$Jour","final_df$Creneau_Horaire","precision","Prix")
  )
})

test_that("good_dataframe names are correct", {
  grid <- main()
  data("train")
  expect_equal(names(good_dataframe(train[1,],grid)),
               c("long1", "lat1", "long2", "lat2",
                 "case_depart", "case_arrivee",
                 "week_day", "hour", "price", "passenger" ))
})

test_that("discretisation_dataframe names are correct", {
  grid <- main()
  data("train")
  expect_equal(names(discretisation_dataframe(good_dataframe(train[1,],grid))),
               c("final_df$IDs","final_df$Passagers",
                 "final_df$Jour","final_df$Creneau_Horaire","precision","Prix"))
})

test_that("transform_row returns a vector", {
  data("train")
  expect_equal(round(transform_row(train[1,])),
               c(-74, 41, -74,  41,   1,  17,   1))
})

test_that("predict returns a price", {
  data("train")
  expect_equal(round(predict(transform_row(train[1,]))), 2)
})
