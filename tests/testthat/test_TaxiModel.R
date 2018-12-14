
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

