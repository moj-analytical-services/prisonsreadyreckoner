# This script tests functions for creating marginal changes in Crown Court
# disposals.


test_that("the Crown module will produce output compatible with intended function", {

  # Generate a mini set for illustration.
  ringfenced_lookup <- tibble::tibble(receipt_type_desc = c("ind", "ind", "tew", "tew", "app"),
                           route = c("egp", "effective", "egp", "effective", "app"),
                           ringfenced = c(TRUE, FALSE, TRUE, FALSE, FALSE)
  )
  
  cc_output <- tibble::tibble(date = c("2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01",
                                          "2023-02-01", "2023-02-01", "2023-02-01", "2023-02-01", "2023-02-01", 
                                          "2023-03-01", "2023-03-01", "2023-03-01", "2023-03-01", "2023-03-01"),
                            receipt_type_desc = c("ind", "ind", "tew", "tew", "app",
                                                 "ind", "ind", "tew", "tew", "app",
                                                 "ind", "ind", "tew", "tew", "app"),
                            route = c("egp", "effective", "egp", "effective", "app",
                                      "egp", "effective", "egp", "effective", "app",
                                      "egp", "effective", "egp", "effective", "app"),
                            n_disposals = c(100, 50, 80, 60, 10,
                                            110, 60, 90, 70, 20,
                                            120, 70, 100, 80, 50),
                            dur_disposals = 60 * c(0.5*100, 2*50, 0.4*80, 2*60, 5*10,
                                                   0.4*110, 2.5*60, 0.6*90, 2*70, 7*20,
                                                   0.3*120, 3*70, 0.5*100, 1*80, 3*50)
  )
  
   cc_capacity <- tibble::tibble(date = c("2023-01-01", "2023-02-01", "2023-03-01"),
                                 sitting_days = c((0.5*100 + 2*50 + 0.4*80 + 2*60 + 5*10) / 4,
                                                  (0.4*110 + 2.5*60 + 0.6*90 + 2*70 + 7*20) / 5,
                                                  (0.3*120 + 3*70 + 0.5*100 + 1*80 + 3*50) / 3),
                                 hours_per_day = c(4, 5, 3))
   
   delta <- tibble::tibble(date = c("2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01",
                                    "2023-02-01", "2023-02-01", "2023-02-01", "2023-02-01", "2023-02-01", 
                                    "2023-03-01", "2023-03-01", "2023-03-01", "2023-03-01", "2023-03-01"),
                           receipt_type_desc = c("ind", "ind", "tew", "tew", "app",
                                                 "ind", "ind", "tew", "tew", "app",
                                                 "ind", "ind", "tew", "tew", "app"),
                           route = c("egp", "effective", "egp", "effective", "app",
                                     "egp", "effective", "egp", "effective", "app",
                                     "egp", "effective", "egp", "effective", "app"),
                           n_receipts_delta = c(0, 0, 0, 0, 0,
                                                 10, 20, 30, 40, 10,
                                                 15, 25, 40, 10, 5)
   )
                           
   expect_true(check_cc_inputs(cc_output, cc_capacity))
   
   
   
   # Test that the crown output table is agumented as expected.
   cc_output_act <- augment_crown_output(cc_output, ringfenced_lookup)
   cc_output_exp <- cc_output
   cc_output_exp$ringfenced <- c(c(TRUE, FALSE, TRUE, FALSE, FALSE),
                                 c(TRUE, FALSE, TRUE, FALSE, FALSE),
                                 c(TRUE, FALSE, TRUE, FALSE, FALSE))
   cc_output_exp$hours_non_ringfenced <- c((2*50 + 2*60 + 5*10) * rep(1, 5),
                                           (2.5*60 + 2*70 + 7*20) * rep(1, 5),
                                           (3*70 + 1*80 + 3*50) * rep(1, 5))
   cc_output_exp$backlog_rate = c(c(0, 50, 0, 60, 10) / (2*50 + 2*60 + 5*10),
                                  c(0, 60, 0, 70, 20) / (2.5*60 + 2*70 + 7*20),
                                   c(0, 70, 0, 80, 50) / (3*70 + 1*80 + 3*50))
   cc_output_exp$hours_per_disposal = c(0.5, 2, 0.4, 2, 5,
                                        0.4, 2.5, 0.6, 2, 7,
                                        0.3, 3, 0.5, 1, 3)
   cc_output_exp$n_receipts_delta = 0
   cc_output_exp$n_disposals_ringfenced_delta = 0
   
   expect_equal(cc_output_act, cc_output_exp)
   
   
  
   # Test that the Crown capacity table is augmented as expected.
   cc_output <- cc_output_act
   cc_capacity_act <- augment_cc_capacity(cc_capacity, cc_output)
   cc_capacity_exp <- cc_capacity
   cc_capacity_exp$hours_ringfenced_base <- c(0.5*100 + 0.4*80,
                                              0.4*110 + 0.6*90,
                                              0.3*120 + 0.5*100)
   cc_capacity_exp$capacity_base = c(0.5*100 + 2*50 + 0.4*80 + 2*60 + 5*10,
                                     0.4*110 + 2.5*60 + 0.6*90 + 2*70 + 7*20,
                                     0.3*120 + 3*70 + 0.5*100 + 1*80 + 3*50)
   
   cc_capacity_exp$sitting_days_delta <- 0
   cc_capacity_exp$hours_ringfenced_delta <- 0
   cc_capacity_exp$capacity_delta <- 0

   expect_equal(cc_capacity_act, cc_capacity_exp)
   
   
   
   # Test that the disposals correctly acknowledge additional receipts and
   # sitting days.
   cc_output <- add_cc_receipts_delta(cc_output, delta)
   cc_capacity <- add_cc_sitting_days(cc_capacity_act, 100, "2023-02-01")
   cc_capacity <- calculate_hours_ringfenced_delta(cc_output, cc_capacity)
   
   cc_disposals_act <- calculate_cc_disposals_delta(cc_output, cc_capacity)
   cc_disposals_exp <- tibble::tibble(date = c("2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01",
                                               "2023-02-01", "2023-02-01", "2023-02-01", "2023-02-01", "2023-02-01", 
                                               "2023-03-01", "2023-03-01", "2023-03-01", "2023-03-01", "2023-03-01"),
                                      receipt_type_desc = c("ind", "ind", "tew", "tew", "app",
                                                            "ind", "ind", "tew", "tew", "app",
                                                            "ind", "ind", "tew", "tew", "app"),
                                      route = c("egp", "effective", "egp", "effective", "app",
                                                "egp", "effective", "egp", "effective", "app",
                                                "egp", "effective", "egp", "effective", "app"),
                                      ringfenced = c(TRUE, FALSE, TRUE, FALSE, FALSE,
                                                     TRUE, FALSE, TRUE, FALSE, FALSE,
                                                     TRUE, FALSE, TRUE, FALSE, FALSE),
                                      n_receipts_delta = c(0, 0, 0, 0, 0,
                                                           10, 20, 30, 40, 10,
                                                           15, 25, 40, 10, 5),
                                      n_disposals_delta = c(c(0,  (0*100 - 0) * 50 / (2*50 + 2*60 + 5*10),                   0,  (0*100 - 0) * 60 / (2*50 + 2*60 + 5*10), (0*100 - 0) * 10 / (2*50 + 2*60 + 5*10)),
                                                            c(10, (5*100 - (0.4*10 + 0.6*30)) * 60 / (2.5*60 + 2*70 + 7*20), 30, (5*100 - (0.4*10 + 0.6*30)) * 70 / (2.5*60 + 2*70 + 7*20), (5*100 - (0.4*10 + 0.6*30)) * 20 / (2.5*60 + 2*70 + 7*20)),
                                                            c(15, (3*100 - (0.3*15 + 0.5*40)) * 70 / (3*70 + 1*80 + 3*50),   40, (3*100 - (0.3*15 + 0.5*40)) * 80 / (3*70 + 1*80 + 3*50), (3*100 - (0.3*15 + 0.5*40)) * 50 / (3*70 + 1*80 + 3*50)))
   )
   
   expect_equal(cc_disposals_act, cc_disposals_exp)
   
})
