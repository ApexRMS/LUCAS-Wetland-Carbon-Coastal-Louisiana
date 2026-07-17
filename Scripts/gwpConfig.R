# Shared GWP-variant configuration for the parallel GWP-100 / GWP-20 scenario chains
# ApexRMS

gwpVariants <- list(
  GWP100 = list(name = "GWP100", factor = 27,   label = "GWP100", suffix = "Revised GWP-100"),
  GWP20  = list(name = "GWP20",  factor = 79.7, label = "GWP20",  suffix = "Revised GWP-20")
)

# Appends a variant tag to a scenario name.
# style = "bracket": internal sub-scenario names, extends a trailing [ ... ]
#   qualifier the same way step3b/step3d already build up compound names,
#   e.g. "SF Flow Pathways [Base Flows, Add Methane]" -> "...Add Methane, GWP100]"
# style = "suffix": outward-facing final scenario names,
#   e.g. "Basin Baseline" -> "Basin Baseline - Revised GWP-100"
vTag <- function(base, variant, style = c("bracket", "suffix")) {
  style <- match.arg(style)
  if (style == "suffix") return(paste0(base, " - ", variant$suffix))
  if (grepl("\\]$", base)) return(sub("\\]$", paste0(", ", variant$label, "]"), base))
  paste0(base, " [", variant$label, "]")
}

# Exact-match replacement for the ambiguous grep(scenarioList$Name)+max(ScenarioId) pattern
getScenarioExact <- function(proj, name) {
  sl <- scenario(proj, summary = TRUE, results = TRUE)
  ids <- sl$ScenarioId[sl$Name == name]
  if (length(ids) != 1) stop("Expected exactly 1 scenario named '", name, "', found ", length(ids))
  scenario(proj, scenario = ids)
}
