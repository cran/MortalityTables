#' Plot multiple mortality tables (life tables) in one plot
#'
#' \code{plotMortalityTables} prints multiple life tables (objects of child classes of \code{mortalityTable}) in one log-linear plot, with a legend showing the names of the tables.
#'
#' @param data First life table to be plotted. Either a \code{data.frame} generated by \code{makeQxDataFrame} or a \code{mortalityTable} object
#' @param ... Additional life tables to be plotted (if \code{data} is a \code{mortalityTable} object)
#' @param aes Optional aesthetics to append or override the default. The default aesthetics will always be applied first and provide defaults for x, y and color. This argument can be used to override the defaults or append other aesthetics.
#' @param xlim X-axis limitatation (as a two-element vector)
#' @param ylim Y-axis limitatation (as a two-element vector)
#' @param xlab X-axis label (default: "Alter")
#' @param ylab Y-axis label (default: "Sterbewahrscheinlichkeit q_x relativ zu ....")
#' @param title The plot title
#' @param legend.position The position of the legend (default is \code{c(0.9,0.1)})
#' @param legend.justification The justification of the legend (default is \code{c(1,)})
#' @param legend.key.width The keywith of the lines in the  legend (default is \code{unit(25,"mm")})
#' @param legend.title Title of the legend (\code{NULL} to hide)
#' @param ages Plot only the given ages
#' @param log Display y axes in logarithmic scale (default: TRUE)
#'
#' @examples
#' # Load the Austrian census data
#' mortalityTables.load("Austria_Annuities")
#' mortalityTables.load("Austria_Census")
#'
#' # Plot some select census tables in a log-linear plot (plot called
#' # with mortalityTable objects is equla to calling plotMortalitytTables directly)
#' plot(mort.AT.census.1869.male, mort.AT.census.1869.female,
#'      mort.AT.census.1971.male, mort.AT.census.1971.female,
#'      mort.AT.census.2011.male, mort.AT.census.2011.female,
#'      title="Austrian census tables",
#'      ylab=expression(q[x]), xlab="Age",
#'      xlim=c(0,90),
#'      legend.position=c(0.95,0.05))
#'
#' # To compare period or cohort life tables, use the YOB and Period arguments:
#' plot(AVOe2005R.male, AVOe2005R.female, AVOe1996R.male, AVOe1996R.female,
#'     Period = 2018, title = "Austrian Annuity Tables, Period 2018")
#' plot(AVOe2005R.male, AVOe2005R.female, AVOe1996R.male, AVOe1996R.female,
#'     YOB = 2000, title = "Austrian Annuity Tables for cohort YOB=2000")
#'
#' @import scales
#' @export
plotMortalityTables = function(
    data, ...,
    aes = NULL,
    ages = NULL,
    legend.title = "Sterbetafel",
    xlim=NULL, ylim=NULL,
    xlab=NULL, ylab=NULL,
    title = "",
    legend.position = c(0.9,0.1), legend.justification = c(1, 0),
    legend.key.width = unit(25, "mm"),
    log = TRUE
) {
    if (!is.data.frame(data)) {
        data = makeQxDataFrame(data, ...);
    }
    if (missing(xlab)) xlab = "Alter";
    if (missing(ylab)) ylab = expression(paste("Sterbewahrscheinlichkeit ", q[x]));

    if (!is.null(ages)) {
        data = data[data$x %in% ages,]
    }

    if (log) {
        data = subset(data, y > 0)
    }
    pl = ggplot(data, aes(x = x, y = y, color = group))
    if (!is.null(aes)) {
        pl = pl + aes
    }
    pl = pl +
        theme_bw() +
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            legend.title = element_text(size = 12, face = "plain"),
            # legend in bottom right corner of the plot
            legend.justification = legend.justification, legend.position = legend.position,
            # No box around legend entries
            legend.key = element_blank(),
            legend.key.width = legend.key.width,

            legend.background = element_rect(colour = "gray50", linetype = "solid")
        ) +
        geom_line(na.rm = TRUE)

    if (log) {
        pl = pl + scale_y_log10(
            breaks = scales::trans_breaks('log10', function(x) 10^x),
            labels = scales::trans_format('log10', scales::math_format(10^.x))
            #minor_breaks = log(c(sapply(x, function(x) seq(0, x, x/10))), 10)
        ) +
            annotation_logticks(sides = "lr")
    }

    pl = pl + scale_x_continuous(
        minor_breaks = function(limits) seq(max(round(min(limits)), 0), round(max(limits)), 1)
    ) +
        coord_cartesian(xlim = xlim, ylim = ylim) +
        labs(x = xlab, y = ylab, colour = legend.title);

    if (title != "") {
        pl = pl + ggtitle(title);
    }
    pl
}

globalVariables(c("x", "y", ".x", "group"))


#
# plotMortalityTables(mort.AT.census.1869.male, mort.AT.census.1869.female, mort.AT.census.2011.male, mort.AT.census.2011.female, AVOe2005R.male, AVOe2005R.female, YOB=1972,title="Austrian Tables, YOB=1972 (for cohort tables)")
#
# plotMortalityTables(mort.AT.census.2001.male, AVOe2005R.male, YOB=1972, title="Comparison Austrian Tables")
#  plotMortalityTables(getCohortTable(AVOe2005R.male, YOB=1972), getCohortTable(AVOe2005R.male, YOB=2016), title="Comparison Austrian Tables")
