# Dynamic Clustering (modified)

This repository contains a modified version of the "Dynamic Clustering" Shiny example.

Changes:
- After the initial clustering, newly added points are drawn as black open circles (pch=1, col='black').
- When points have been added since the last clustering, a message appears on the plot:
  "Points have been added since the last clustering. Click 'Go' to incorporate these new points in a new clustering."
- A "Go" button is provided to re-run clustering and incorporate newly-added points.
- A "Clear" button resets points and clustering.

How to run locally:
1. Ensure R is installed.
2. From an R session:
   install.packages("shiny")
   install.packages("mclust")
3. In the project directory run:
   library(shiny)
   runApp("app.R")

Or just from shell:
   R -e "shiny::runApp('.')"

Notes:
- The app attempts an automatic initial clustering once there are >= 2 points. After that, additional points are left unclustered visually until you press "Go".
- The app uses `mclust::Mclust` and `mclust::mclust2Dplot` to display clusters.
