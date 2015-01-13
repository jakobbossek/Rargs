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
    parsed.args = initParsedArguments(arg.set)

    # FIXME: this is rather ugly. Could we make this more R-like?
    # FIXME: if is.flag is true, no type is required or set to logical automatically
    i = 1L
    while (i <= length(raw.args)) {
        raw.arg = raw.args[i]
        #catf("Processing with argument '%s'", raw.arg)
        if (!(raw.arg %in% arg.names) && !(raw.arg %in% arg.shortcuts)) {
            stopf("Unknown argument '%s' provided.", raw.arg)
        }
        # get matching argument
        arg.id = which(raw.arg == arg.names)
        if (length(arg.id) == 0) {
            arg.id = which(raw.arg == arg.shortcuts)
        }
        arg = arg.set$arguments[[arg.id]]

        # handle case of flag, i.e., argument without value
        if (arg$is.flag) {
            i = i + 1
            # 'value' of flags is always TRUE
            parsed.args[[raw.arg]] = TRUE
            next
        }

        # argument required, but no default
        i = i + 1L
        raw.val = raw.args[i]
        #FIXME: casting to type
        parsed.args[[raw.arg]] = raw.val

        i = i + 1L
    }

    # check if all required arguments are passed?
    unsatisfied = getRequiredAndNotGiven(arg.set, names(parsed.args))
    if (length(unsatisfied) > 0L) {
        #FIXME: move this paste stuff to specific function pasteVector
        stopf("There are missing required arguments: '%s'", do.call(paste, c(as.list(unsatisfied), sep = ", ")))
    }
    class(parsed.args) = "RargsParsedArguments"
    return(parsed.args)
}

# Helper function.
#
# Initialize result object with default values for arguments with default
# values specified.
#
# @param arg.set [RargsArgumentSet]
#  Argument set.
# @return [list]
initParsedArguments = function(arg.set) {
    parsed.args = list()
    for (argument in arg.set$arguments) {
        if (!is.null(argument$default)) {
            parsed.args[argument$name] = argument$default
        }
    }
    return(parsed.args)
}


# Helper function.
#
# Checks whether all required arguments are set.
#
# @param arg.set [RargsArgumentSet]
#  Argument set.
# @param arg.names [character]
#  Vector of arguments specified by the user.
# @return [character(1)]
#  Character vector of argument long names, which are required, but not provided.
getRequiredAndNotGiven = function(arg.set, arg.names) {
    assertClass(arg.set, "RargsArgumentSet")
    assertCharacter(arg.names)

    args.required = extractSubList(arg.set$arguments, "required")
    required.args.names = extractSubList(arg.set$arguments, "name")
    required.arg.names = arg.names[which(args.required)]
    unsatisfied = setdiff(required.arg.names, arg.names)
    return(unsatisfied)
}
