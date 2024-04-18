main <- function() {
  folder.path <- "C:\\Users\\erezm\\coding\\stat215b\\final_project\\"
  source(paste0(folder.path, "stat215b\\party.stance.R"))
  #
  load(paste0(folder.path, "data.Robj"))
  data %<>% enrich.party.stance
  data %<>% filter.party.stance
  data %>% analyze.placement.within.party(0.05, 0.05)
  data %>% analyze.opposide.side.perception(0.05, 2000)
}
library(magrittr)
"%notin%" <- Negate("%in%")
"%.%" <- function(f, g, .dp=0)
{
  f <- substitute(f)
  g <- substitute(g)
  
  ## treatment of f()
  if("%.%" == all.names(f)[1])
  {
    ## f() is a %.% expression, expand it by a deeper call of %.%.
    f <- eval(as.call(append(as.list(f), c(.dp=.dp+1))))
  }
  else
  {
    ## f() is the deepest function, (i.e., the left most syntax of
    ## a %.% chain), insert ... as its first argument.
    f <- as.call(append(as.list(f), quote(...), 1L))
  }
  
  ## expands g() to g(f(...)) by treating f(...) as the 1st argument
  g <- as.call(append(as.list(g), f, 1L))
  
  ## pack up and return
  if(.dp > 0)
  {
    ret <- g # a deeper call of %.% return expanded expression
  }
  else
  {
    ## the top call of %.% build a function enclosing g(f(...))
    ret <- function(...) {}
    body(ret) <- g
    ## treat the function as if it was defined from outside.
    environment(ret) <- parent.frame()
  }
  ret
}
nunique <- function(x) length(unique(x))
revsetdiff <- function(x, y) setdiff(y, x)
dsetdiff <- function(x, y) list(setdiff(x, y), setdiff(y, x))
na.set <- function(x, val) ifelse(is.na(x), val, x)
find.different.chars <- function(x, y) {
  stopifnot(nchar(x) == nchar(y))
  which(sapply(1:nchar(x), function(i) substr(x, i, i) != substr(y, i, i)))
}
in.place.tapply <- function(x, fac, fun, ...) {
  # tapply resulting in same length as x, then insert result back into x at correct order
  stopifnot(length(x) == length(fac))
  t <- tapply(x, fac, fun, ...)
  out <- numeric(length(x))
  for (r in names(t)) {
    out[fac == r] <- t[[r]]
  }
  out
}
quantile.plot <- function(x, y, by, bins=10, by.bins=3) {
  stopifnot(missing(bins))
  fac <- findInterval(x, quantile(x, p=1:(bins-1)/bins))
  qx <- tapply(x, fac, mean)
  qy <- tapply(y, fac, mean)
  plot(qx, qy)
  lines(qx, qy)
}
