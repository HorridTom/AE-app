# Packages the app needs that nothing else here loads directly.
#
# autospc only suggests ggrepel and ggpp, but it needs them to place the
# centre-line labels and draw their arrows; without them it falls back to
# basic labels. Naming them here makes renv record them in renv.lock, so that
# they are installed when the app is deployed. This code is never run.
if (FALSE) {
  library(ggrepel)
  library(ggpp)
}
