#' Method to parse arguments.
#'
#' @param arg.set [\code{RargsArgumentSet}]\cr
#'   Set of arguments.
#' @param raw.args [\code{character}]\cr
#'   Character vector of raw command arguments, i. e., arguments passed by the
#'   user to the program. Default ist \code{\link[base]{commandArgs}(trailingOnly = TRUE)}.
#' @return [\code{RargsParsedArguments}]
#'   List of parsed arguments.
#' @export
parseArguments = function(arg.set, raw.args = commandArgs(trailingOnly = TRUE)) {
    assertClass(arg.set, "RargsArgumentSet")
    assertCharacter(raw.args)

    arg.names = extractSubList(arg.set$arguments, "name")
    arg.shortcuts = extractSubList(arg.set$arguments, "shortcut")

    # FIXME: this should be initialized with default values.
    parsed.args = list()

    # FIXME: this is rather ugly. Could we make this more R-like?
    i = 1L
    while (i <= length(raw.args)) {
        raw.arg = raw.args[i]
        catf("Processing with argument '%s'", raw.arg)
        if (!(raw.arg %in% arg.names) && !(raw.arg %in% arg.shortcuts)) {
            stopf("Unknown argument '%s' provided.", raw.arg)
        }
        arg.id = which(raw.arg == arg.names)
        #print(arg.id)
        if (length(arg.id) == 0) {
            arg.id = which(raw.arg == arg.shortcuts)
        }
        arg = arg.set$arguments[[arg.id]]
        #print(arg)

        if (arg$is.flag) {
            i = i + 1
            # 'value' of flags is always TRUE
            parsed.args[[raw.arg]] = TRUE
            next
        }

        # argument required, but no default

        i = i + 1L
        raw.val = raw.args[i]
        parsed.args[[raw.arg]] = raw.val

        i = i + 1L
    }

    # check if all required arguments are passed?
    arg.required = extractSubList(arg.set$arguments, "required")
    required.arg.names = arg.names[which(arg.required)]
    unsatisfied = setdiff(required.arg.names, names(parsed.args))
    if (length(unsatisfied) > 0L) {
        #FIXME: move this paste stuff to specific function pasteVector
        stopf("There are missing required arguments: '%s'", do.call(paste, c(as.list(unsatisfied), sep = ", ")))
    }
    class(parsed.args) = "RargsParsedArguments"
    return(parsed.args)
}