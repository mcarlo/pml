# options(help_type='html', browser=c('gnome-www-browser','midori')[2])
options(repos=c('http://cran.rstudio.com', 'http://cran.case.edu')[1],
        lib='/usr/local/lib/R/site-library')
installPac <- function(p) install.packages(p,
  repos=options('repos'),
  lib='/usr/local/lib/R/site-library')
updatePac <- function(checkBuilt=FALSE, ...)
  update.packages(repos=options('repos'),
                  instlib='/usr/local/lib/R/site-library',
                  checkBuilt=checkBuilt, ...)
spar <- function(mar=if(!axes)
                 c(2.25+bot-.45*multi,2+left,.5+top+.25*multi,.5+rt) else
                 c(3.25+bot-.45*multi,3.5+left,.5+top+.25*multi,.5+rt),
                 lwd = if(multi)1 else 1.75,
                 mgp = if(!axes) mgp=c(.75, .1, 0) else
                 if(multi) c(1.5, .365, 0) else c(2.4-.4, 0.475, 0),
                 tcl = if(multi)-0.25 else -0.4, xpd=FALSE,
                 bot=0, left=0, top=0, rt=0, ps=if(multi) 14 else 10,
                 mfrow=NULL, axes=TRUE, cex.lab=1.25, cex.axis=1.15,
                 ...) {
  multi <- length(mfrow) > 0
  par(mar=mar, lwd=lwd, mgp=mgp, tcl=tcl, ps=ps, xpd=xpd,
      cex.lab=cex.lab, cex.axis=cex.axis, ...)
  if(multi) par(mfrow=mfrow)
}

# Usage:
# <<echo=FALSE>>=
# knitrSet()
# knitrSet('basename') e.g. knitrSet('regressionChapter')
# @
knitrSet <- function(basename=NULL, w=4, h=3,
                     fig.align='center', fig.show='hold', fig.pos='htbp',
                     fig.lp=paste('fig', basename, sep=':'), dev='pdf',
                     tidy=FALSE, error=FALSE,
                     messages=c('messages.txt', 'console'),
                     width=61, decinline=5, size=NULL, cache=FALSE,
                     echo=TRUE, results='markup') {
  messages <- match.arg(messages)
  ## Specify e.g. dev=c('pdf','png') or dev=c('pdf','postscript')
  ## to produce two graphics files for each plot
  ## But: dev='CairoPNG' is preferred for png
  if(length(basename)) basename <- paste(basename, '-', sep='')

  options(width=width)
  ## fills Sweavel boxes when font size is \small and svmono.cls
  ## is in effect (use 65 without svmono)

  render_listings()
  if(messages != 'console') {
  unlink(messages) # Start fresh with each run
	hook_log = function(x, options) cat(x, file=messages, append=TRUE)
	knit_hooks$set(warning = hook_log, message = hook_log)
  }
  else opts_chunk$set(message=FALSE, warning=FALSE)
  if(length(size)) opts_chunk$set(size = size)
  
  if(length(decinline)) {
    rnd <- function(x, dec) if(!is.numeric(x)) x else round(x, dec)
    formals(rnd) <- list(x=NULL, dec=decinline)
    knit_hooks$set(inline = rnd)
  }
  knit_hooks$set(par=function(before, options, envir)
                 if(before && options$fig.show != 'none') {
                   p <- c('bty','mfrow','ps','bot','top','left','rt','lwd',
                          'mgp','tcl', 'axes','xpd')
                   pars <- opts_current$get(p)
                   pars <- pars[!is.na(names(pars))]
                   ## knitr 1.6 started returning NULLs for unspecified pars
                   i <- sapply(pars, function(x) length(x) > 0)
                   if(any(i)) do.call('spar', pars[i]) else spar()
                 })
  opts_knit$set(
    aliases=c(h='fig.height', w='fig.width', cap='fig.cap', scap='fig.scap'))
    #eval.after = c('fig.cap','fig.scap'),
    #error=error)  #, keep.source=keep.source (TRUE))
  opts_chunk$set(fig.path=basename, fig.align=fig.align, w=w, h=h,
                 fig.show=fig.show, fig.lp=fig.lp, fig.pos=fig.pos,
                 dev=dev, par=TRUE, tidy=tidy, out.width=NULL, cache=cache,
                 echo=echo, results=results, error=error)
  hook_chunk = knit_hooks$get('chunk')
  ## centering will not allow too-wide figures to go into left margin
  knit_hooks$set(chunk = function(x, options) { 
    res = hook_chunk(x, options) 
    if (options$fig.align != 'center') return(res) 
    gsub('\\{\\\\centering (\\\\includegraphics.+)\n\n\\}', 
         '\\\\centerline{\\1}', res) 
  }) 
}
## see http://yihui.name/knitr/options#package_options

## Use caption package options to control caption font size

## Function to layout multiple lattice graphs into a 2x2 matrix
pmlattice <- function(..., nc=2) {
  w <- list(...)
  if(!inherits(w[[1]], 'trellis')) w <- w[[1]]
  n <- length(w)
  nr <- ceiling(n / nc)
  row <- 1
  col <- 0
  for(i in 1 : n) {
    col <- col + 1
    if(col > nc) {
      col <- 1
      row <- row + 1
    }
    print(w[[i]], more=i < n, split=c(col, row,  nc, nr))
  }
}
