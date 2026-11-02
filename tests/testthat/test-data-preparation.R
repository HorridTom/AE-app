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

test_that("Region and country codes are looked up from names", {
  orgs <- organisation_lookup(ae_data_test)
  expect_equal(organisation_code(orgs, "England", "National"), "E")
  expect_equal(organisation_code(orgs, "Scotland", "National"), "S")
  expect_equal(organisation_code(orgs, "England", "Regional", "London"), "Lo")
  expect_equal(
    organisation_code(orgs, "Scotland", "Regional", "Lothian"),
    "S08000024"
  )
})

test_that("Menus list each country's organisations in order", {
  orgs <- organisation_lookup(ae_data_test)
  expect_equal(country_names(orgs), c("England", "Scotland"))
  expect_equal(
    region_names(orgs, "England"),
    c("London", "Midlands", "North of England", "South of England")
  )
  choices <- provider_choices(orgs, "England")
  expect_equal(names(choices), sort(names(choices), method = "radix"))
  expect_equal(
    choices[["Chelsea And Westminster Hospital NHS Foundation Trust"]],
    "RQM"
  )
  expect_false("A210H" %in% choices)
})

# Made-up providers: P1 has changed name twice; P2 and P3 have the same name
renamed <- tibble::tribble(
  ~Prov_Code , ~Prov_Name       , ~Period      ,
  "P1"       , "First Name"     , "2016-01-01" ,
  "P1"       , "First Name"     , "2016-02-01" ,
  "P1"       , "Second Name"    , "2016-03-01" ,
  "P1"       , "Current Name"   , "2016-04-01" ,
  "P1"       , "Current Name"   , "2016-05-01" ,
  "P2"       , "Walk In Centre" , "2016-01-01" ,
  "P3"       , "Walk In Centre" , "2016-05-01"
) |>
  mutate(
    Period = as.Date(Period),
    Country = "England",
    Nat_Code = "E",
    Region = "London",
    Reg_Code = "Lo"
  )

test_that("Providers are listed under their latest name", {
  orgs <- organisation_lookup(renamed)
  expect_equal(provider_name(orgs, "P1"), "Current Name")
  expect_equal(
    provider_choices(orgs, "England"),
    c(
      "Current Name" = "P1",
      "Walk In Centre (P2)" = "P2",
      "Walk In Centre (P3)" = "P3"
    )
  )
})

test_that("A provider's earlier names are listed with their dates", {
  earlier <- earlier_names(renamed, "P1")
  expect_equal(earlier$Prov_Name, c("First Name", "Second Name"))
  expect_equal(
    earlier_names_note(renamed, "P1"),
    paste0(
      "Previously: First Name (January 2016 to February 2016); ",
      "Second Name (March 2016)."
    )
  )
  expect_equal(earlier_names_note(renamed, "P2"), "")
})
