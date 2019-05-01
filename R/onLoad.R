#https://kbroman.org/pkg_primer/
# http://r-pkgs.had.co.nz/

.onLoad <- function(libname, pkgname) {
  rJava::.jpackage(pkgname, lib.loc=libname)
}

# http://www.legendu.net/en/blog/call-java-in-r/
# https://www.developer.com/java/ent/using-the-rjava-r-package-to-do-more.html
# https://www.r-bloggers.com/a-primer-in-using-java-from-r-part-2/
