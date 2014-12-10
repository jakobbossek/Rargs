#' Make argument object.
#'
#' @param name [\code{character(1)}]\cr
#'   Name of the argument.
#' @param shortcut [\code{character(1)}]\cr
#'   Shortcut name of the argument. Default is \code{NULL}, i. e., no shortcut.
#' @param alias [\code{character}]\cr
#'   Character vector of alias names for \code{name} argument. Default is \code{NULL},
#'   i. e., no aliases at all.
#' @param is.flag [\code{logical(1)}]\cr
#'   Set this to \code{TRUE}, if the argument is a flag, i. e., it has no value.
#' @param default [\code{any}]\cr
#'   Default value for the argument. Keep in mind, that this must be of the corresponding
#'   \code{type}. If no default value is defined, which is the default, the user has
#'   to provide a value.
#' @param type [\code{character(1)}]\cr
#'   The type of the argument. Argument values are coerced to the type. Keep in mind, that
#'   this argument is neglected if \code{is.flag} is set to \code{TRUE}.
#FIXME: possible types?
#' @param required [\code{logical(1)}]\cr
#'   Set this to \code{TRUE}, if the user must provide this argument. Default ist \code{FALSE}.
#' @param help [\code{character(1)}]\cr
#'   Helping text.
#' @return [\code{RargsArgument}] Argument object.
#' @export
makeArgument = function(name,
    shortcut = NULL, alias = NULL, is.flag = FALSE,
    default = NULL, type = NULL, required = FALSE,
    help) {
    assertCharacter(name)
    if (!is.null(shortcut))
        assertCharacter(shortcut)
    if (!is.null(alias))
        assertCharacter(shortcut, any.missing = FALSE)
    assertFlag(is.flag)
    if (!is.flag && is.null(type))
        stopf("You need to specify a type for argument '%s'", name)
    if (!is.flag) {
        assertChoice(type, choices = c("numeric", "logical", "integer", "character"))
        if (!is.null(default)) {}
        #FIXME: check if default value corresponds to type
    }
    assertFlag(required)
    assertCharacter(help)

    makeS3Obj(
        name = name,
        shortcut = shortcut,
        alias = alias,
        is.flag = is.flag,
        default = default,
        type = type,
        required = required,
        help = help,
        classes = "RargsArgument"
    )
}