context("parse arguments")

test_that("parseArguments parses correctly", {
    arg.set = makeArgumentSet(
        makeArgument("num1", type = "numeric", help = ""),
        makeArgument("num2", type = "integer", help = ""),
        makeArgument("char1", alias = c("c1"), type = "character", help = ""),
        makeArgument("flag1", is.flag = TRUE, help = "")
    )

    parsed.args = parseArguments(
        arg.set,
        raw.args = c("num1", "1.0", "num2", "1L", "char1", "test", "flag1")
    )

    expect_is(parsed.args, "RargsParsedArguments")
    expect_true(is.list(parsed.args))

    expect_error(parseArguments(
        arg.set,
        raw.args = c("num1", "num2", "2L")
    ))

    # FIXME: type should be set automatically if
    arg.set = makeArgumentSet(
        makeArgument("flag1", type = "logical", shortcut = "f1", required = TRUE, help = "")
    )

    expect_error(parseArguments(
        arg.set,
        raw.args = c())
    )
})