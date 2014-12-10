#' Make set of arguments.
#'
#' @param ... [\code{RargsArgument}]\cr
#'    Arguments.
#' @return [\code{RargsArgumentSet}]
#'    Set of arguments ready to pass to \code{\link{parseArguments}}.
#FIXME: add example
#' @export
makeArgumentSet = function(...) {
    arguments = list(...)
    assertList(arguments, types = "RargsArgument", min.len = 1L)
    argument.names = extractSubList(arguments, "name")
    if (any(duplicated(argument.names))) {
        stopf("Arguments must be uniquely named!")
    }
    makeS3Obj(
        arguments = arguments,
        classes = "RargsArgumentSet"
    )
}