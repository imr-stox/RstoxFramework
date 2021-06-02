library(testthat)
library(RstoxFramework)

write("newArgumentFiles", file.path(tempdir(), "heihei.txt"))

# We have currently three test projects:
options(Ncpus = 3L)

test_check("RstoxFramework")

readLines(file.path(tempdir(), "heihei.txt"))
