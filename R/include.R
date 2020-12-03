## read_include_c <- function(filename) {
##   d <- readLines(filename)

##   re1 <- "^[[:alnum:]_*]+ ([[:alnum:]_]+)(.+)"
##   i1 <- grep(re1, d)
##   i2 <- grep("^}$", d)
##   if (length(i1) != length(i2)) {
##     stop("Parse error for ", filename)
##   }
##   name <- sub(re1, "\\1", d[i1])
##   defn <- setNames(vcapply(seq_along(i1), function(k)
##     paste(d[i1[[k]]:i2[[k]]], collapse = "\n")), name)
##   decl <- sub("^([^{]*?)\\s*\\{.*", "\\1;", defn)

##   list(
##     names = name,
##     data = list(names = name,
##                 declarations = decl,
##                 definitions = defn,
##                 filename = filename))
## }


## read_include_r <- function(filename) {
##   e <- new.env(parent = baseenv())
##   sys.source(filename, e)
##   list(names = names(e),
##        data = list(source = readLines(filename)))
## }

read_include_dust <- function(filename) {
  dat <- decor::cpp_decorations(files = filename)
  code <- dat$context[dat$decoration == "odin.dust::register"]
  names <- vcapply(code, function(x) decor::parse_cpp_function(x)$name)
  if (length(names) == 0) {
    stop("Did not find any functions decorated with '[[odin.dust::register]]'")
  }
  list(names = names,
       data = list(source = paste(readLines(filename), collapse = "\n")))
}
