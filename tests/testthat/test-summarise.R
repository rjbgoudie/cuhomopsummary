
test_that('Basic analysis works', {
  df <- data.frame(person_id = 1:10, gender_concept_id = 0, birth_date = as.Date('2020-01-01'))
  res <- summarise_omop_table('person', df)
  expect_equal(res$count, 10)
  expect_true(res$cols$incomplete[res$cols$column == 'gender_concept_id'])
})
