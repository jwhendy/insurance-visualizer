insurance-visualizer
====================

This repo stores the code and files necessary to reproduce the R/Shiny web app located here: http://spark.rstudio.com/jwhendy/insurance-visualizer/. To re-create the app, you will need to install:

- [R](http://www.r-project.org/)
- [RStudio](http://www.rstudio.com/)
- [The shiny package](http://cran.r-project.org/web/packages/shiny/index.html)
- The other packages used in `server.R` (namely, `ggplot2`, `markdown`, and `plyr`)

I make no promises on the elegance of the code! I've been using R for about 2 years, but primarily for relatively simple data manipulation, analysis, and plots at work. This is certainly my biggest project completed to-date and the skills above and beyond my day job requirements are developed purely in hobby time!

For background on my motivations, each year my company sends out glossy, elegant booklets with the upcoming year's insurance plan information prior to annual enrollment where each employee picks a plan for health coverage. Three years ago I tired of trying to estimate what I would pay for various expense scenarios either in my head or via a kludgy Excel spreadsheet, so I created some simple plots showing what an employee would actually pay for various levels of medical expenses incurred. I did this for my own plan (employee + family), and then did the same for the other insurance options (employee only, employee + spouse, and employee + child[ren]).

When I was all done, I turned it into a nice presentation and screencast walkthrough, making it available to others. It was quite well received and became an annual hobby. Last year, plans began to include split deductibles, which *really* complicates things. I resorted to contour plots and "lowest cost plan maps" showing the best plan for various combinations of expense, but it was still pretty complex to talk through and required a lot of plots.

Recently, RStudio released `shiny`, which lets one make web apps fairly easily with R, so I knew this year's analysis would be interactive! I'm fairly happy with the result, and was able to host it on an internal server at my place of work. The version hosted on RStudio's servers (URL above) has the plan names anonymized and all values slightly "tweaked" in order to avoid an issues, just in case HR doesn't want me revealing this information.

While it's not perfect, I can't fathom how individuals are supposed to make financially important decisions like this based on tables, no matter how glossy the paper on which they are printed! From searching online, however, it appears that a simple list of the deductible, out of pocket maximum, and premium are pretty much all that's provided with any plan. This was my attempt to bring about a change to that, at least within my sphere of influence!

Feedback and improvements are welcome!