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
# IMPORTANT: a result scenario does NOT share its parent's Name -- SyncroSim appends a
# date/time suffix, e.g. parent "X" produces results named "X ([<parentId>] @ <date> <time>)".
# This is why the original grep()+max(ScenarioId) pattern used substring matching (every
# result's name contains the parent's name as a prefix) despite the ambiguity risk that
# motivated this replacement (e.g. "Basin Baseline" matching "Basin Baseline - Revised
# GWP-100" too). The fix here uses the structured ParentId column instead of string
# matching: find the unique top-level scenario (ParentId is NA) with an exact Name match,
# then return the highest-ScenarioId row among it and its actual result children (i.e.
# the latest result if it's been run, or the parent itself if it hasn't).
getScenarioExact <- function(proj, name) {
  sl <- scenario(proj, summary = TRUE)
  parentIds <- sl$ScenarioId[sl$Name == name & is.na(sl$ParentId)]
  if (length(parentIds) != 1) stop("Expected exactly 1 top-level scenario named '", name, "', found ", length(parentIds))
  parentId <- parentIds[1]
  childIds <- sl$ScenarioId[!is.na(sl$ParentId) & sl$ParentId == parentId]
  scenario(proj, scenario = max(c(parentId, childIds)))
}

# Toggle: set to FALSE to skip re-running a single-cell scenario (or, for
# calculateDecayRates(), skip the whole calibration) when it already has at
# least one result scenario. Useful when iterating on downstream steps without
# wanting to re-run slow spinup/single-cell scenarios every time. Set back to
# TRUE to force everything to (re)run regardless of existing results.
rerunScenariosWithResults <- TRUE

# TRUE if `name` already has at least one associated result scenario (a
# scenario that has been run() at least once). Uses ParentId, not Name, for the
# same reason as getScenarioExact() above -- results don't share their parent's Name.
hasResults <- function(proj, name) {
  sl <- scenario(proj, summary = TRUE)
  parentIds <- sl$ScenarioId[sl$Name == name & is.na(sl$ParentId)]
  if (length(parentIds) != 1) return(FALSE)
  any(!is.na(sl$ParentId) & sl$ParentId == parentIds[1])
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
