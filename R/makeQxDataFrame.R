#' Converts one or multiple mortality table objects to a data frame that can be
#' plotted by \code{plotMortalityTables} or \code{plotMortalityTableComparisons}
#'
#' It is not required to call this function manually, \code{plotMortalityTables}
#' will automatically do it if object derived from class \code{mortalityTable}
#' are passed.
#'
#' @param ... Life tables (objects of classes derived from \code{mortalityTable})
#' @param YOB desired year of birth to be plotted as cohort life table (default: 1972)
#' @param Period desired observation year to be plotted (default: NA). If both
#'        \code{YOB} and \code{Period} are given, a period comparison is generated.
#' @param reference Reference life table, used to show relative death
#'        probabilities (i.e. the q_x for all ages are divided by the
#'        corresponding probabilities of the reference table)
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' makeQxDataFrame(AVOe2005R.male, AVOe2005R.female, YOB = 1975)
#'
#' @export
makeQxDataFrame = function(..., YOB = 1972, Period = NA, reference = NULL) {
    # Allow lists of tables to be passed, too. unlist should always return
    # a list of tables, even if nested lists or only tables are passed as arguments!
    data = unlist(list(...));
    if (is.null(data)) return(data.frame(x = double(), y = double(), group = character()))

    reference_ages = NULL;
    # browser()

    if (missing(Period)) {
        # If reference is given, normalize all probabilities by that table!
        if (!missing(reference) && !is.null(reference)) {
            reference_ages = ages(reference);
            reference = deathProbabilities(reference, YOB = YOB);
        }
        data = lapply(data, function(t) {
            normalize_deathProbabilities(
                if (is.data.frame(t@data$dim) || is.list(t@data$dim)) {
                       data.frame(x = ages(t), y = `names<-`(deathProbabilities(t, YOB = YOB), NULL), group = t@name, as.data.frame(t@data$dim))
                } else {
                       data.frame(x = ages(t), y = `names<-`(deathProbabilities(t, YOB = YOB), NULL), group = t@name)
                },
                reference = reference,
                referenceAges = reference_ages)
        });
    } else {
        if (!missing(reference) && !is.null(reference)) {
            reference_ages = ages(reference);
            reference = periodDeathProbabilities(reference, Period = Period);
        }
        data = lapply(data, function(t) {
            normalize_deathProbabilities(
                if (is.data.frame(t@data$dim) || is.list(t@data$dim)) {
                    data.frame(x = ages(t), y = `names<-`(periodDeathProbabilities(t, Period = Period), NULL), group = t@name, as.data.frame(t@data$dim))
                } else {
                    data.frame(x = ages(t), y = `names<-`(periodDeathProbabilities(t, Period = Period), NULL), group = t@name)
                },
                reference = reference,
                referenceAges = reference_ages)
        });
    }

    names(data) = NULL

    data <- as.data.frame(do.call("rbind.expand", data))
    data
}

normalize_deathProbabilities = function(data, reference = NULL, referenceAges = NULL) {
    if (missing(reference) || missing(referenceAges) || is.null(reference) || is.null(referenceAges)) {
        return(data);
    }
    # Find which ages exist in both and obtain those indices from the data and the reference list:
    useages = intersect(data[,"x"], referenceAges)
    dataindices = match(useages, data[,"x"])
    refindices = match(useages, referenceAges)

    # Find which ages in data do NOT exist in the reference ages (and are thus NOT normalized at all)
    # Print a warning!
    missingrefs = setdiff(data[,"x"], referenceAges)
    if (length(missingrefs) > 0) {
        warning("Reference mortality table does not contain ages ",
                missingrefs,
                " required for normalization. These ages will not be normalized!")
    }

    # Now divide the data by the corresponding entries from the reference list
    data[dataindices, "y"] = data[dataindices, "y"] / reference[refindices]
    data
}


rbind.expand = function(df1, df2, ..., fill = NA) {
    # browser()
    if (missing(df2) || is.null(df2))
        return(df1)

    df2.clmiss = setdiff(colnames(df1), colnames(df2))
    df1.clmiss = setdiff(colnames(df2), colnames(df1))

    df1[df1.clmiss] = fill
    df2[df2.clmiss] = fill

    rbind.expand(rbind(df1, df2), ..., fill = fill)
}
