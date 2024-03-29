#' @include mortalityTable.R fillAges.R
NULL

pensionTableProbArrange = function(x, q, i, qi, r, ap, api, qp, h, qw, yx, qg, as.data.frame = TRUE, table, ...) {
    if (as.data.frame) {
        data.frame(x, q, i, qi, r, ap, api, qp, h, qw, yx, qg)
    } else {
        states = c("a", "i", "p", "d")
        transProb = array(0, dim = c(length(states), length(states), length(x)), dimnames = list(states, states, x))

        transProb["a", "a", ] = (1 - i - q) * (1 - ap);
        transProb["a", "i", ] = i;
        transProb["a", "p", ] = (1 - i - q) * ap;
        transProb["a", "d", ] = q;

        transProb["i", "a", ] = r;
        transProb["i", "i", ] = (1 - qi - r) * (1 - api);
        transProb["i", "p", ] = (1 - qi - r) * api;
        transProb["i", "d", ] = qi;

        transProb["p", "p", ] = 1 - qp;
        transProb["p", "d", ] = qp;

        transProb["d", "d", ] = 1;

        list(transitionProbabilities = transProb, widows = data.frame(x, h, qw, yx))
    }
}


#' Class pensionTable
#'
#' Class \code{pensionTable} is the (virtual) base class for all pensions
#' tables. It contains the name and some general values applying to all
#' types of tables. In particular, it holds individual tables for each of the
#' transition probabilities. Possible states are:
#' \itemize{
#'     \item active: healty, no pension, typically paying some kin of premium
#'     \item incapacity: disablity pension, in most cases permanent, not working, early pension
#'     \item retirement: old age pension, usually starting with a fixed age
#'     \item dead \itemize{
#'       \item Widow/widower pension
#'     }
#' }
#' Correspondingly, the following transition probabilities can be given:\describe{
#'     \item{qxaa}{death probability of actives (active -> dead)}
#'     \item{ix}{invalidity probability (active -> incapacity)}
#'     \item{qix}{death probability of invalid (invalid -> dead)}
#'     \item{rx}{reactivation probability (incapacity -> active)}
#'     \item{apx}{retirement probability (active -> retirement), typically 1 for a fixed age}
#'     \item{qpx}{death probability of retired (retired -> dead)}
#'     \item{hx}{probability of a widow at moment of death (dead -> widow), y(x) age difference}
#'     \item{qxw}{death probability of widows/widowers}
#'     \item{qgx}{death probability of total group (irrespective of state)}
#'     \item{invalids.retire}{Flag to indicate whether invalid persons retire
#'           like active (one death probability for all retirees) or whether
#'           they stay invalid until death with death probabilities specific to
#'           invalids.}
#' }
#'
#' @slot qx     Death probability table of actives (derived from mortalityTable)
#' @slot ix     Invalidity probability of actives (derived from mortalityTable)
#' @slot qix    Death probability table of invalids (derived from mortalityTable)
#' @slot rx     Reactivation probability of invalids (derived from mortalityTable)
#' @slot apx    Retirement probability of actives (derived from mortalityTable)
#' @slot qpx    Death probability of old age pensioners (derived from mortalityTable)
#' @slot hx     Probability of a widow at the moment of death (derived from mortalityTable)
#' @slot qwy    Death probability of widow(er)s (derived from mortality Table)
#' @slot yx     Age difference of the widow to the deceased
#' @slot qgx    Death probability of whole group (derived from mortalityTable), irrespective of state
#' @slot invalids.retire    Whether invalids retire like actives or stay invalid until death
#' @slot probs.arrange A function that takes the individual transition probabilities of all the components and creates one object (a data.frame or a list) that will be returned by the method \code{transitionProbabilities}. The default arranges all tables without further modification. However, some pension tables (like the german Heubeck table) require the total mortality to be recalculated from the individual mortalities of actives and disabled. In this case, the function assigned to this slot will also calculate that total probability.
#'
#' @export pensionTable
#' @exportClass pensionTable
pensionTable = setClass(
    "pensionTable",
    slots = list(
        qx    = "mortalityTable",
        ix    = "mortalityTable",
        qix   = "mortalityTable",
        rx    = "mortalityTable",
        apx   = "mortalityTable",
        qpx   = "mortalityTable",
        hx    = "mortalityTable",
        qwy   = "mortalityTable",
        yx    = "mortalityTable",
        qgx   = "mortalityTable",
        invalids.retire = "logical",
        probs.arrange = "function"
    ),
    prototype = list(
        invalids.retire = FALSE,
        probs.arrange = pensionTableProbArrange
    ),
    contains = "mortalityTable"
)

#' Return all transition probabilities of the pension table (generational probabilities)
#'
#' @param object A pension table object (instance of a \code{\linkS4class{pensionTable}} class)
#' @param ... Currently unused
#' @param YOB Year of birth
#' @param ages Desired age range (if NULL, the probabilities of the age range provided by the table will be returned), missing ages will be filled with NA
#' @param Period Observation year to calculate period transition probabilities.
#'               If given, this arguments overrides the \code{YOB} parameter
#'               and this function returns period transition probabilities.
#'               If this argument is not given or is null, then this function
#'               returns generational transition probabilities.
#' @param as.data.frame Whether the return value should be a data.frame or an
#'                      array containing transition matrices
#' @param retirement Override the retirement transition probabilities of the
#'                   pension table. Possible values are:\itemize{
#'                     \item Single age (describing a deterministric retirement at the given age)
#'                     \item mortalityTable object: transition probabilities for retirement
#'                   }
#' @param invalids.retire Override the \code{\linkS4class{pensionTable}}'s
#'                        \code{invalids.retire} flag, which indicates whether
#'                        invalids retire like actives (i.e. same death
#'                        probabilities after retirement) or stay invalid until
#'                        death.
#' @param OverallMortality Whether the overall mortality should be returned for actives, or the active mortality
#'
#' @examples
#' pensionTables.load("USA_PensionPlans")
#' transitionProbabilities(RP2014.male, YOB = 1962)
#' transitionProbabilities(RP2014.male, Period = 1955)
#' transitionProbabilities(RP2014.male, Period = 2025)
#'
#' @exportMethod transitionProbabilities
setGeneric("transitionProbabilities", function(object, ...) standardGeneric("transitionProbabilities"));

#' @describeIn transitionProbabilities Return all transition probabilities of the pension table for the generation YOB
setMethod("transitionProbabilities", "pensionTable",
          function(object, YOB = 1982, ..., ages = NULL, OverallMortality = FALSE, Period = NULL, retirement = NULL,
                   invalids.retire = object@invalids.retire, as.data.frame = TRUE) {
              if (!missing(Period) && !is.null(Period)) {
                  return(periodTransitionProbabilities(
                      object, ..., ages = ages, Period = Period, retirement = retirement,
                      invalids.retire = invalids.retire,
                      as.data.frame = as.data.frame))
              }
              x   = if (is.null(ages)) ages(object@qx) else  ages;
              q   = deathProbabilities(object@qx, ..., ages = ages, YOB = YOB);
              i   = deathProbabilities(object@ix, ..., ages = ages, YOB = YOB);
              qi  = deathProbabilities(object@qix, ..., ages = ages, YOB = YOB);
              r   = deathProbabilities(object@rx, ..., ages = ages, YOB = YOB);
              apTab = object@apx
              if (!missing(retirement) && !is.null(retirement)) {
                  if (inherits(retirement, "mortalityTable")) {
                      apTab = retirement
                  } else if (is.numeric(retirement) && length(retirement) == 1) {
                      # Single retirement age given
                      apTab = mortalityTable.once(
                          transitionAge = retirement - 1, ages = x,
                          name = paste("Retirement at age ", retirement))
                  } else {
                      warning("transitionProbabilities: Invalid value for ",
                              "argument retirement. Allowed are only: numeric ",
                              "(retirement age) or mortalityTable (retirement ",
                              "probabilities). Given: ", retirement);
                      apTab = mortalityTable.zeroes(ages = x)
                  }
              }
              ap  = deathProbabilities(apTab, ..., ages = ages, YOB = YOB);

              if (!missing(retirement) && !is.null(retirement)) {
                  if (inherits(retirement, "mortalityTable")) {
                  } else if (is.numeric(retirement) && length(retirement) == 1) {
                  } else {
                      warning("transitionProbabilities: Invalid value for ",
                              "argument retirement. Allowed are only: numeric ",
                              "(retirement age) or mortalityTable (retirement ",
                              "probabilities). Given: ", retirement);
                      apTab = mortalityTable.zeroes(ages = x)
                  }
              }
              if (invalids.retire) {
                  api = ap
              } else {
                  api = deathProbabilities(mortalityTable.zeroes(ages = x), ..., ages = ages, YOB = YOB)
              }
              qp  = deathProbabilities(object@qpx, ..., ages = ages, YOB = YOB);
              h   = deathProbabilities(object@hx, ..., ages = ages, YOB = YOB);
              qw  = deathProbabilities(object@qwy, ..., ages = ages, YOB = YOB);
              yx  = deathProbabilities(object@yx, ..., ages = ages, YOB = YOB);
              qg  = deathProbabilities(object@qgx, ..., ages = ages, YOB = YOB);
              if (!OverallMortality) {
                  object@probs.arrange(x, q, i, qi, r, ap, api, qp, h, qw, yx, qg, as.data.frame = as.data.frame, table = object)
              } else {
                  # Gesamttafel, i.e. actives, invalids and pensioners have the same mortality qg
                  object@probs.arrange(x, qg, i, qg, r, ap, api, qg, h, qw, yx, qg, as.data.frame = as.data.frame, table = object)
              }
          })

#' Return all period transition probabilities of the pension table
#'
#' @param object A pension table object (instance of a \code{\linkS4class{pensionTable}} class)
#' @param Period Observation year
#' @param ... Currently unused
#' @param ages Desired age range (if NULL, the probabilities of the age range provided by the table will be returned), missing ages will be filled with NA
#' @param retirement Override the retirement transition probabilities of the pension table. Possible values are:\itemize{
#'                   \item Single age (describing a deterministric retirement at the given age)
#'                   \item mortalityTable object: transition probabilities for retirement
#'                   }
#' @param invalids.retire Override the \code{\linkS4class{pensionTable}}'s
#'                        \code{invalids.retire} flag, which indicates whether
#'                        invalids retire like actives (i.e. same death
#'                        probabilities after retirement) or stay invalid until
#'                        death.
#' @param as.data.frame Whether the return value should be a data.frame or an array containing transition matrices
#' @param OverallMortality Whether the overall mortality should be returned for actives, or the active mortality
#'
#' @examples
#' pensionTables.load("USA_PensionPlans")
#' # transitionProbabilities internally calls periodTransitionProbabilities
#' # if a Period is given:
#' transitionProbabilities(RP2014.male, Period = 1955)
#' periodTransitionProbabilities(RP2014.male, Period = 1955)
#' periodTransitionProbabilities(RP2014.male, Period = 2025)
#'
#' @exportMethod periodTransitionProbabilities
setGeneric("periodTransitionProbabilities", function(object, ...) standardGeneric("periodTransitionProbabilities"));


#' @describeIn periodTransitionProbabilities Return all transition probabilities of the pension table for the period Period
setMethod("periodTransitionProbabilities", "pensionTable",
          function(object, Period = 2017, ..., ages = NULL, OverallMortality = FALSE, retirement = NULL, invalids.retire = object@invalids.retire, as.data.frame = TRUE) {
              x   = if (is.null(ages)) ages(object@qx) else  ages;
              q   = periodDeathProbabilities(object@qx, ..., ages = ages, Period = Period);
              i   = periodDeathProbabilities(object@ix, ..., ages = ages, Period = Period);
              qi  = periodDeathProbabilities(object@qix, ..., ages = ages, Period = Period);
              r   = periodDeathProbabilities(object@rx, ..., ages = ages, Period = Period);
              apTab = object@apx
              if (!missing(retirement) && !is.null(retirement)) {
                  if (inherits(retirement, "mortalityTable")) {
                      apTab = retirement
                  } else if (is.numeric(retirement) && length(retirement) == 1) {
                      # Single retirement age given
                      apTab = mortalityTable.once(
                          transitionAge = retirement - 1, ages = x,
                          name = paste("Retirement at age ", retirement))
                  } else {
                      warning("transitionProbabilities: Invalid value for ",
                              "argument retirement. Allowed are only: numeric ",
                              "(retirement age) or mortalityTable (retirement ",
                              "probabilities). Given: ", retirement);
                      apTab = mortalityTable.zeroes(ages = x)
                  }
              }
              ap = deathProbabilities(apTab, ..., ages = ages, Period = Period)
              if (invalids.retire) {
                  api = ap
              } else {
                  api = deathProbabilities(mortalityTable.zeroes(ages = x), ..., ages = ages, Period = Period)
              }
              qp  = periodDeathProbabilities(object@qpx, ..., ages = ages, Period = Period);
              h   = periodDeathProbabilities(object@hx, ..., ages = ages, Period = Period);
              qw  = periodDeathProbabilities(object@qwy, ..., ages = ages, Period = Period);
              yx  = periodDeathProbabilities(object@yx, ..., ages = ages, Period = Period);
              qg  = periodDeathProbabilities(object@qgx, ..., ages = ages, Period = Period);
              if (!OverallMortality) {
                  object@probs.arrange(x, q, i, qi, r, ap, api, qp, h, qw, yx, qg, as.data.frame = as.data.frame, table = object)
              } else  {
                  object@probs.arrange(x, qg, i, qg, r, ap, api, qg, h, qw, yx, qg, as.data.frame = as.data.frame, table = object)
              }
          })


if (FALSE) {
    pensionTables.load("Austria_AVOe2008P")
    transitionProbabilities(AVOe2008P.male, YOB = 1977, as.data.frame = FALSE)
    epP = transitionProbabilities(EttlPagler.male, YOB = 1982)
#    avoe08p =
        transitionProbabilities(AVOe2008P.male, YOB = 1977, as.data.frame = TRUE)
avoe08p.period = periodTransitionProbabilities(AVOe2008P.male, Period = 2007, as.data.frame = TRUE)

pensionTables.list(package = "MortalityTablesPrivate")
pensionTables.load("Austria_AVOe1999P")
}

