## ---- echo = FALSE, message = FALSE-------------------------------------------
library(maat)
library(knitr)
library(kableExtra)

## ---- echo = FALSE, out.width = "900px"---------------------------------------
include_graphics("assessment.svg")

## ---- echo = FALSE, out.width = "900px"---------------------------------------
include_graphics("routing_T1T2.svg")

## ---- echo = FALSE, out.width = "900px"---------------------------------------
include_graphics("routing_T2T3.svg")

## ---- echo = FALSE, out.width = "900px"---------------------------------------
include_graphics("routing_T3.svg")

## ---- results = "hide"--------------------------------------------------------
assessment_structure <- createAssessmentStructure(
  n_test  = 3,
  n_phase = 2,
  route_limit_below = 1,
  route_limit_above = 2
)

## ---- results = "hide"--------------------------------------------------------
cor_v <- matrix(.8, 3, 3)
diag(cor_v) <- 1

set.seed(1)
examinee_list <- simExaminees(
  N = 10,
  mean_v = c(0, 0.5, 1.0),
  sd_v   = c(1, 1, 1),
  cor_v  = cor_v,
  assessment_structure = assessment_structure,
  initial_grade = "G4",
  initial_phase = "P1",
  initial_test  = "T1"
)

## ---- echo = FALSE------------------------------------------------------------
fn <- system.file("extdata", "module_definition_MATH_normal_N500.csv", package = "maat")
d  <- read.csv(fn)
kable_styling(kable(d))

## -----------------------------------------------------------------------------
fn <- system.file("extdata", "module_definition_MATH_normal_N500.csv", package = "maat")
module_list <- loadModules(
  fn = fn,
  base_path = system.file(package = "maat"),
  assessment_structure = assessment_structure,
  examinee_list = examinee_list
)

## -----------------------------------------------------------------------------
cut_scores <- list(
  G3 = c(-1.47, -0.55, 0.48),
  G4 = c(-1.07, -0.15, 0.88),
  G5 = c(-0.67,  0.25, 1.28),
  G6 = c(-0.27,  0.65, 1.68),
  G7 = c( 0.13,  1.05, 2.08),
  G8 = c( 0.53,  1.45, 2.48)
)

## ---- results = "hide", message = FALSE---------------------------------------
library(TestDesign)
config <- createShadowTestConfig(
  interim_theta = list(method = "MLE"),
  final_theta = list(method = "MLE")
)

## ---- results = "hide", message = FALSE---------------------------------------
set.seed(1)
maat_output_CI <- maat(
  examinee_list          = examinee_list,
  assessment_structure   = assessment_structure,
  module_list            = module_list,
  config                 = config,
  cut_scores             = cut_scores,
  overlap_control_policy = "within_test",
  transition_policy      = "CI",
  combine_policy         = "conditional",
  transition_CI_alpha    = 0.05
)
set.seed(1)
maat_output_difficulty <- maat(
  examinee_list          = examinee_list,
  assessment_structure   = assessment_structure,
  module_list            = module_list,
  config                 = config,
  cut_scores             = cut_scores,
  overlap_control_policy = "within_test",
  transition_policy      = "pool_difficulty_percentile",
  combine_policy         = "conditional",
  transition_CI_alpha         = 0.05,
  transition_percentile_lower = 0.05,
  transition_percentile_upper = 0.95
)

## ---- message = FALSE, out.width = "100%", fig.width = 8----------------------
plot(maat_output_CI, type = "route")

## ---- message = FALSE, out.width = "100%", fig.width = 8----------------------
plot(maat_output_difficulty, type = "route")

## ---- fig.width = 6, fig.height = 3, out.width = "100%"-----------------------
plot(
  x           = maat_output_CI,
  type        = "correlation",
  theta_range = c(-4, 4),
  main        = c("Fall", "Winter", "Spring"))

## ---- fig.width = 6, fig.height = 3, out.width = "100%"-----------------------
plot(
  x           = maat_output_difficulty,
  type        = "correlation",
  theta_range = c(-4, 4),
  main        = c("Fall", "Winter", "Spring"))

## ---- out.width = "100%"------------------------------------------------------
plot(
  x = maat_output_CI,
  type = "audit",
  examinee_id = 1
)

