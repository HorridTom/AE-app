test_that("England regions are grouped into four, with their own codes", {
  england <- ae_data_test[ae_data_test$Country == "England", ]
  expect_setequal(
    unique(england$Region),
    c("London", "Midlands", "North of England", "South of England")
  )
  expect_setequal(unique(england$Reg_Code), c("Lo", "Mi", "No", "So"))
  expect_false(any(
    c("Lo", "Mi", "No", "So") %in%
      ae_data_test$Reg_Code[ae_data_test$Country == "Scotland"]
  ))
})

test_that("Scotland has no type 1 or admissions data", {
  scotland <- ae_data_test[ae_data_test$Country == "Scotland", ]
  expect_true(all(is.na(scotland$Att_Typ1)))
  expect_true(all(is.na(scotland$E_Adm_All_ED)))
  expect_false(any(grepl("^NHS ", scotland$Region)))
})

test_that("Organisation codes are looked up from names", {
  orgs <- organisation_lookup(ae_data_test)
  expect_equal(organisation_code(orgs, "England", "National"), "E")
  expect_equal(organisation_code(orgs, "Scotland", "National"), "S")
  expect_equal(organisation_code(orgs, "England", "Regional", "London"), "Lo")
  expect_equal(
    organisation_code(orgs, "Scotland", "Regional", "Lothian"),
    "S08000024"
  )
  expect_equal(
    organisation_code(
      orgs,
      "England",
      "Provider",
      "Chelsea And Westminster Hospital NHS Foundation Trust"
    ),
    "RQM"
  )
})

test_that("Menus list each country's organisations in order", {
  orgs <- organisation_lookup(ae_data_test)
  expect_equal(country_names(orgs), c("England", "Scotland"))
  expect_equal(
    region_names(orgs, "England"),
    c("London", "Midlands", "North of England", "South of England")
  )
  providers <- provider_names(orgs, "England")
  expect_equal(providers, sort(providers, method = "radix"))
  expect_false("University Hospital Ayr" %in% providers)
})
