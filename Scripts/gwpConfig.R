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

# Exact-match replacement for the ambiguous grep(scenarioList$Name)+max(ScenarioId) pattern.
# Two things the original grep()+max(ScenarioId) pattern relied on that this must preserve:
#   1. A result scenario keeps the SAME Name as its parent (distinguished only by
#      ScenarioId), so a scenario that has been run() one or more times will have
#      multiple rows sharing this exact name -- that's expected, not an error.
#   2. When multiple rows share the name (parent + result(s), or several results from
#      repeated runs), we want the most recent one, i.e. max(ScenarioId).
# `results` is intentionally omitted here (defaults to FALSE = return everything,
# both definition and result scenarios) -- results=TRUE would exclude scenarios that
# have been created but not yet run(), which is what originally broke this.
# The actual bug this function exists to fix is grep()'s ambiguous SUBSTRING matching
# (e.g. "Basin Baseline" matching "Basin Baseline - Revised GWP-100" too) -- using an
# exact `==` match on Name fixes that while still allowing multiple ScenarioId matches.
getScenarioExact <- function(proj, name) {
  sl <- scenario(proj, summary = TRUE)
  ids <- sl$ScenarioId[sl$Name == name]
  if (length(ids) == 0) stop("Expected at least 1 scenario named '", name, "', found 0")
  scenario(proj, scenario = max(ids))
}

# Toggle: set to FALSE to skip re-running a single-cell scenario (or, for
# calculateDecayRates(), skip the whole calibration) when it already has at
# least one result scenario. Useful when iterating on downstream steps without
# wanting to re-run slow spinup/single-cell scenarios every time. Set back to
# TRUE to force everything to (re)run regardless of existing results.
rerunScenariosWithResults <- TRUE

# TRUE if `name` already has at least one associated result scenario (a
# scenario that has been run() at least once).
hasResults <- function(proj, name) {
  existingResults <- scenario(proj, summary = TRUE, results = TRUE)
  name %in% existingResults$Name
}

# Runs `name` via run(), unless rerunScenariosWithResults is FALSE and results
# already exist for this exact scenario name.
runIfNeeded <- function(proj, name, ...) {
  if (!rerunScenariosWithResults && hasResults(proj, name)) {
    message("Skipping run() for '", name, "' -- results already exist (set rerunScenariosWithResults <- TRUE in gwpConfig.R to force a rerun)")
    return(invisible(NULL))
  }
  run(proj, scenario = name, ...)
}
