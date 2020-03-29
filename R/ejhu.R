#' create cumulative_events instance for state data from NYT
#' @param src a tibble
#' @param eventtype character(1) 'confirmed' or 'deaths'
#' @param statename character(1) state name
#' @examples
#' nyt = nytimes_state_data()
#' mass = cumulative_events_nyt_state(nyt, eventtype = "confirmed",
#'    statename = "Massachusetts")
#' mass
#' imass = form_incident_events(trim_from(mass, "2020-03-01"))
#' plot(imass)
#' @export
cumulative_events_nyt_state = function (src, eventtype = "confirmed", 
      statename = "Massachusetts") {
    stopifnot(statename %in% src$state)
    cur = src %>% dplyr::filter(subset == eventtype & state == statename)
    cumul = cur$count
    dates = cur$date
    ans = list(count = cumul, dates = dates)
    attr(ans, "ProvinceState") = statename
    attr(ans, "source") = "NYT"
    attr(ans, "dtype") = "cumulative"
    class(ans) = c("cumulative_events", "covid_events")
    ans
}

#' print for a covid_events instance
#' @param x covid_events instance
#' @param \dots not used
#' @export
print.covid_events = function(x, ...)  {
cursrc = attr(x, "source")
cat(sprintf("%s event data for %s, %s to %s", attr(x, "dtype"),
  attr(x, ifelse(cursrc == "JHU", "alpha3", "ProvinceState")), min(x$dates), max(x$dates)), "\n")
cat("use plot() to visualize.\n")
}

#' trim early part of series, generic
#' @param x covid_events instance
#' @param date string representing date in yyyy-mm-dd form
#' @export
trim_from = function(x, date) UseMethod("trim_from")

#' trim early part of series
#' @param x covid_events instance
#' @param date string representing date in yyyy-mm-dd form
#' @export
trim_from.covid_events = function(x, date = "2020-02-15") {
  chk = substr(date, 1, 4)
  if (chk != "2020") warning("expecting year 2020 in trim date, please check")
  st = lubridate::as_date(date)
  ok = which(x$dates >= st)
  x$dates = x$dates[ok]
  x$count = x$count[ok]
  x
}

#' plot for covid_events
#' @param x covid_events instance
#' @param main defaults to NULL, can be character(1)
#' @param ylab defaults to NULL, can be character(1)
#' @param xlab defaults to NULL, can be character(1)
#' @param \dots not used
#' @export
plot.covid_events = function (x, main=NULL, ylab=NULL, xlab=NULL,  ...) {
  sr = attr(x, "source")
  if (is.null(main)) main = paste0(attr(x, "dtype"), 
            " events for ", attr(x, ifelse(sr=="JHU", "alpha3", "ProvinceState")))
  if (is.null(ylab)) ylab = paste0(attr(x, "dtype"))
  if (is.null(xlab)) xlab = "Date"
  plot(x$dates, x$count, main=main, ylab=ylab, xlab=xlab, ...)
}

#' constructor for covid_events
#' @param src as retrieved with enhanced_jhu_data
#' @param eventtype character(1) 'confirmed' or 'deaths'
#' @param alpha3 character(1) code for country
#' @param ProvinceState character(1) for province, default to NULL
#' @export
cumulative_events_ejhu = function(src, eventtype = "confirmed", 
   alpha3="USA", ProvinceState=NULL) {
 cur = src %>% filter(subset == eventtype &
                alpha3Code == alpha3 )
 if (!is.null(ProvinceState))
   cur = cur %>% filter(ProvinceState == ProvinceState)
 cumul = cur$count
 dates = cur$date
 ans = list(count=cumul, dates=dates)
 attr(ans, "alpha3") = alpha3
 attr(ans, "source") = "JHU"
 attr(ans, "ProvinceState") = ProvinceState
 attr(ans, "dtype") = "cumulative"
 class(ans) = c("cumulative_events", "covid_events")
 ans
}

#' transform cumulative data to daily incidence
#' @param cum cumulative events instance
#' @examples
#' suppressWarnings({dat = enriched_jhu_data()})
#' cusa = cumulative_events_ejhu(dat, eventtype="confirmed",
#'  alpha3="USA")
#' cusa
#' cusa = trim_from(cusa, "2020-03-01")
#' iusa = form_incident_events(cusa)
#' plot(iusa)
#' gt = R0::generation.time("gamma", c(3.5, 4.8))
#' est1 = R0::estimate.R( iusa$count, GT=gt,
#'    t=iusa$dates, begin=iusa$dates[1], end=iusa$dates[length(iusa$dates)],
#'    methods="EG")
#' est1
#' plot2(est1)
#' plotfit2(est1)
#' @export
form_incident_events = function(cum) {
 stopifnot(inherits(cum, "cumulative_events"))
 ans = list(count=diff(cum$count), dates=cum$dates[-1])
 attributes(ans) = attributes(cum)
 attr(ans, "dtype") = "incident"
 class(ans) = c("incident_events", "covid_events")
 ans
}
