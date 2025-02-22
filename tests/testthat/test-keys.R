
test_that("validate_pk detects missing primary key column", {
  df <- data.frame(id = 1:5, value = letters[1:5])
  expect_warning(validate_pk(df, "test_table"), "La columna 'test_table_pk' no existe en la hoja 'test_table'.")
})

test_that("validate_pk detects NA values in primary key column", {
  df <- data.frame(test_table_pk = c(1, 2, NA, 4, 5), value = letters[1:5])
  expect_warning(validate_pk(df, "test_table"), "La columna 'test_table_pk' en la hoja 'test_table' no puede tener valores NA o duplicados.")
})

test_that("validate_pk detects duplicate values in primary key column", {
  df <- data.frame(test_table_pk = c(1, 2, 2, 4, 5), value = letters[1:5])
  expect_warning(validate_pk(df, "test_table"), "La columna 'test_table_pk' en la hoja 'test_table' no puede tener valores NA o duplicados.")
})

test_that("validate_pk passes when primary key column is correct", {
  df <- data.frame(test_table_pk = 1:5, value = letters[1:5])
  expect_silent(validate_pk(df, "test_table"))
})

test_that("validate_fk detects missing foreign key column", {
  df_pk <- data.frame(parent_table_pk = 1:5)
  df_fk <- data.frame(value = letters[1:5])
  expect_warning(validate_fk(df_pk, "parent_table", df_fk, "child_table"), "La columna 'parent_table_fk' no existe en la hoja 'child_table'.")
})

test_that("validate_fk detects foreign key values not in primary key column", {
  df_pk <- data.frame(parent_table_pk = 1:5)
  df_fk <- data.frame(parent_table_fk = c(1, 2, 6, 4, 5))
  expect_warning(validate_fk(df_pk, "parent_table", df_fk, "child_table"), "Todos los valores de la columna 'parent_table_fk' en la hoja 'child_table' han de estar en la columna 'parent_table_pk' de la hoja 'parent_table'.")
})

test_that("validate_fk passes when foreign key column is correct", {
  df_pk <- data.frame(parent_table_pk = 1:5)
  df_fk <- data.frame(parent_table_fk = c(1, 2, 3, 4, 5))
  expect_silent(validate_fk(df_pk, "parent_table", df_fk, "child_table"))
})
