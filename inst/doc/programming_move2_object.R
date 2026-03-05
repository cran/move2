## ----include = FALSE--------------------------------------------------------------
library(move2)
library(assertthat)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----error=T----------------------------------------------------------------------
data <- mt_sim_brownian_motion(1:3)[c(1, 3, 2, 6, 4, 5), ]
assert_that(mt_is_time_ordered(data))

## ----ttt, eval=FALSE--------------------------------------------------------------
# data %>%
#   group_by(mt_track_id()) %>%
#   group_split() %>%
#   purrr::map(~ {
#     # track wise operations can be executed here (use .x to refer to the `move2` object)
#   }) %>%
#   mt_stack()

## ----ttat, eval=FALSE-------------------------------------------------------------
# data %>%
#   group_by(mt_track_id()) %>%
#   group_modify(
#     ~{
#       # operations go here
#     }
#   )

