summary.phenopix <- function(object, ...) {
    cat('\nData\n')
    print(summary(object$data))
    cat('\nPredicted\n')
    print(summary(object$fit$fit$predicted))
    cat('\nFormula\n')
    print(object$fit$fit$formula)
    cat('\nThresholds\n')
    print(object$metrics)
}