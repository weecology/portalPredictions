

y = interpolated_abundances
allow.multiplicative.trend = TRUE
model = "ZZZ"
damped = NULL
alpha = NULL
beta = NULL
gamma = NULL
phi = NULL
additive.only = FALSE
lambda = NULL
biasadj = FALSE
lower = c(rep(1e-04, 3), 0.8)
upper = c(rep(0.9999, 3), 0.98)
opt.crit = "lik"
nmse = 3
bounds = "both"
ic = "aicc"
restrict = TRUE
use.initial.values = FALSE

ets <- function (y, model = "ZZZ", damped = NULL, alpha = NULL, beta = NULL, 
    gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL, 
    biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999, 
        3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", 
        "mae"), nmse = 3, bounds = c("both", "usual", "admissible"), 
    ic = c("aicc", "aic", "bic"), restrict = TRUE, allow.multiplicative.trend = FALSE, 
    use.initial.values = FALSE, ...) 
{
    opt.crit <- match.arg(opt.crit)
    bounds <- match.arg(bounds)
    ic <- match.arg(ic)
    seriesname <- deparse(substitute(y))
    if (any(class(y) %in% c("data.frame", "list", "matrix", "mts"))) 
        stop("y should be a univariate time series")
    y <- as.ts(y)
    if (missing(model) & is.constant(y)) 
        return(ses(y, alpha = 0.99999, initial = "simple")$model)
    ny <- length(y)
    y <- na.contiguous(y)
    if (ny != length(y)) 
        warning("Missing values encountered. Using longest contiguous portion of time series")
    orig.y <- y
    if (class(model) == "ets" & is.null(lambda)) 
        lambda <- model$lambda
    if (!is.null(lambda)) {
        y <- BoxCox(y, lambda)
        additive.only <- TRUE
    }
    if (nmse < 1 | nmse > 30) 
        stop("nmse out of range")
    m <- frequency(y)
    if (any(upper < lower)) 
        stop("Lower limits must be less than upper limits")
    if (class(model) == "ets") {
        alpha <- max(model$par["alpha"], 1e-10)
        beta <- model$par["beta"]
        if (is.na(beta)) 
            beta <- NULL
        gamma <- model$par["gamma"]
        if (is.na(gamma)) 
            gamma <- NULL
        phi <- model$par["phi"]
        if (is.na(phi)) 
            phi <- NULL
        modelcomponents <- paste(model$components[1], model$components[2], 
            model$components[3], sep = "")
        damped <- (model$components[4] == "TRUE")
        if (use.initial.values) {
            errortype <- substr(modelcomponents, 1, 1)
            trendtype <- substr(modelcomponents, 2, 2)
            seasontype <- substr(modelcomponents, 3, 3)
            e <- pegelsresid.C(y, m, model$initstate, errortype, 
                trendtype, seasontype, damped, alpha, beta, gamma, 
                phi, nmse)
            np <- length(model$par) + 1
            model$loglik <- -0.5 * e$lik
            model$aic <- e$lik + 2 * np
            model$bic <- e$lik + log(ny) * np
            model$aicc <- model$aic + 2 * np * (np + 1)/(ny - 
                np - 1)
            model$mse <- e$amse[1]
            model$amse <- mean(e$amse)
            tsp.y <- tsp(y)
            model$states <- ts(e$states, frequency = tsp.y[3], 
                start = tsp.y[1] - 1/tsp.y[3])
            colnames(model$states)[1] <- "l"
            if (trendtype != "N") 
                colnames(model$states)[2] <- "b"
            if (seasontype != "N") 
                colnames(model$states)[(2 + (trendtype != "N")):ncol(model$states)] <- paste("s", 
                  1:m, sep = "")
            if (errortype == "A") 
                model$fitted <- ts(y - e$e, frequency = tsp.y[3], 
                  start = tsp.y[1])
            else model$fitted <- ts(y/(1 + e$e), frequency = tsp.y[3], 
                start = tsp.y[1])
            model$residuals <- ts(e$e, frequency = tsp.y[3], 
                start = tsp.y[1])
            model$sigma2 <- mean(model$residuals^2, na.rm = TRUE)
            model$x <- orig.y
            model$series <- seriesname
            if (!is.null(lambda)) {
                model$fitted <- InvBoxCox(model$fitted, lambda, 
                  biasadj, var(model$residuals))
                attr(lambda, "biasadj") <- biasadj
            }
            model$lambda <- lambda
            return(model)
        }
        else {
            model <- modelcomponents
            if (missing(use.initial.values)) {
                message("Model is being refit with current smoothing parameters but initial states are being re-estimated.\nSet 'use.initial.values=TRUE' if you want to re-use existing initial values.")
            }
        }
    }
    errortype <- substr(model, 1, 1)
    trendtype <- substr(model, 2, 2)
    seasontype <- substr(model, 3, 3)
    if (!is.element(errortype, c("M", "A", "Z"))) 
        stop("Invalid error type")
    if (!is.element(trendtype, c("N", "A", "M", "Z"))) 
        stop("Invalid trend type")
    if (!is.element(seasontype, c("N", "A", "M", "Z"))) 
        stop("Invalid season type")
    if (m < 1 | length(y) <= m) {
        seasontype <- "N"
    }
    if (m == 1) {
        if (seasontype == "A" | seasontype == "M") 
            stop("Nonseasonal data")
        else substr(model, 3, 3) <- seasontype <- "N"
    }
    if (m > 24) {
        if (is.element(seasontype, c("A", "M"))) 
            stop("Frequency too high")
        else if (seasontype == "Z") {
            warning("I can't handle data with frequency greater than 24. Seasonality will be ignored. Try stlf() if you need seasonal forecasts.")
            substr(model, 3, 3) <- seasontype <- "N"
        }
    }
    if (restrict) {
        if ((errortype == "A" & (trendtype == "M" | seasontype == 
            "M")) | (errortype == "M" & trendtype == "M" & seasontype == 
            "A") | (additive.only & (errortype == "M" | trendtype == 
            "M" | seasontype == "M"))) 
            stop("Forbidden model combination")
    }
    data.positive <- (min(y) > 0)
    if (!data.positive & errortype == "M") 
        stop("Inappropriate model for data with negative or zero values")
    if (!is.null(damped)) {
        if (damped & trendtype == "N") 
            stop("Forbidden model combination")
    }
    n <- length(y)
    npars <- 2L
    if (trendtype == "A" | trendtype == "M") 
        npars <- npars + 2L
    if (seasontype == "A" | seasontype == "M") 
        npars <- npars + m
    if (!is.null(damped)) 
        npars <- npars + as.numeric(damped)
    if (n <= npars + 4L) {
        if (!is.null(damped)) 
            if (damped) 
                warning("Not enough data to use damping")
        if (seasontype == "A" | seasontype == "M") {
            fit <- try(HoltWintersZZ(orig.y, alpha = alpha, beta = beta, 
                gamma = gamma, phi = phi, exponential = (trendtype == 
                  "M"), seasonal = ifelse(seasontype != "A", 
                  "multiplicative", "additive"), lambda = lambda, 
                biasadj = biasadj, warnings = FALSE), silent = TRUE)
            if (!("try-error" %in% class(fit))) {
                fit$call <- match.call()
                fit$method <- as.character(fit)
                fit$series <- deparse(substitute(y))
                return(fit)
            }
            else warning("Seasonal component could not be estimated")
        }
        if (trendtype == "A" | trendtype == "M") {
            fit <- try(HoltWintersZZ(orig.y, alpha = alpha, beta = beta, 
                gamma = FALSE, phi = phi, exponential = (trendtype == 
                  "M"), lambda = lambda, biasadj = biasadj, warnings = FALSE), 
                silent = TRUE)
            if (!("try-error" %in% class(fit))) {
                fit$call <- match.call()
                fit$method <- as.character(fit)
                fit$series <- deparse(substitute(y))
                return(fit)
            }
            else warning("Trend component could not be estimated")
        }
        if (trendtype == "N" & seasontype == "N") {
            fit <- try(HoltWintersZZ(orig.y, alpha = alpha, beta = FALSE, 
                gamma = FALSE, lambda = lambda, biasadj = biasadj, 
                warnings = FALSE), silent = TRUE)
            if (!("try-error" %in% class(fit))) {
                fit$call <- match.call()
                fit$method <- as.character(fit)
                fit$series <- deparse(substitute(y))
                return(fit)
            }
        }
        fit1 <- try(HoltWintersZZ(orig.y, alpha = alpha, beta = beta, 
            gamma = FALSE, phi = phi, exponential = (trendtype == 
                "M"), lambda = lambda, biasadj = biasadj, warnings = FALSE), 
            silent = TRUE)
        fit2 <- try(HoltWintersZZ(orig.y, alpha = alpha, beta = FALSE, 
            gamma = FALSE, phi = phi, exponential = (trendtype == 
                "M"), lambda = lambda, biasadj = biasadj, warnings = FALSE), 
            silent = TRUE)
        if ("try-error" %in% class(fit1)) 
            fit <- fit2
        else if (fit1$sigma2 < fit2$sigma2) 
            fit <- fit1
        else fit <- fit2
        fit$call <- match.call()
        fit$method <- as.character(fit)
        fit$series <- deparse(substitute(y))
        return(fit)
    }
    if (errortype == "Z") 
        errortype <- c("A", "M")
    if (trendtype == "Z") {
        if (allow.multiplicative.trend) 
            trendtype <- c("N", "A", "M")
        else trendtype <- c("N", "A")
    }
    if (seasontype == "Z") 
        seasontype <- c("N", "A", "M")
    if (is.null(damped)) 
        damped <- c(TRUE, FALSE)
    best.ic <- Inf
    for (i in 1:length(errortype)) {
        for (j in 1:length(trendtype)) {
            for (k in 1:length(seasontype)) {
                for (l in 1:length(damped)) {
                  if (trendtype[j] == "N" & damped[l]) 
                    next
                  if (restrict) {
                    if (errortype[i] == "A" & (trendtype[j] == 
                      "M" | seasontype[k] == "M")) 
                      next
                    if (errortype[i] == "M" & trendtype[j] == 
                      "M" & seasontype[k] == "A") 
                      next
                    if (additive.only & (errortype[i] == "M" | 
                      trendtype[j] == "M" | seasontype[k] == 
                      "M")) 
                      next
                  }
                  if (!data.positive & errortype[i] == "M") 
                    next
                  fit <- forecast:::etsmodel(y, errortype[i], trendtype[j], 
                    seasontype[k], damped[l], alpha, beta, gamma, 
                    phi, lower = lower, upper = upper, opt.crit = opt.crit, 
                    nmse = nmse, bounds = bounds)
                  fit.ic <- switch(ic, aic = fit$aic, bic = fit$bic, 
                    aicc = fit$aicc)
                  if (!is.na(fit.ic)) {
                    if (fit.ic < best.ic) {
                      model <- fit
                      best.ic <- fit.ic
                      best.e <- errortype[i]
                      best.t <- trendtype[j]
                      best.s <- seasontype[k]
                      best.d <- damped[l]
                    }
                  }
                }
            }
        }
    }
    if (best.ic == Inf) 
        stop("No model able to be fitted")
    model$m <- m
    model$method <- paste("ETS(", best.e, ",", best.t, ifelse(best.d, 
        "d", ""), ",", best.s, ")", sep = "")
    model$series <- seriesname
    model$components <- c(best.e, best.t, best.s, best.d)
    model$call <- match.call()
    model$initstate <- model$states[1, ]
    model$sigma2 <- mean(model$residuals^2, na.rm = TRUE)
    model$x <- orig.y
    if (!is.null(lambda)) {
        model$fitted <- InvBoxCox(model$fitted, lambda, biasadj, 
            var(model$residuals))
        attr(lambda, "biasadj") <- biasadj
    }
    model$lambda <- lambda
    return(structure(model, class = "ets"))
}
