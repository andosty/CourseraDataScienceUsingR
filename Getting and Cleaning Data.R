
#
#############################

# Since swirl is an R package, you can easily install it by entering a single comman
install.packages("swirl")

# If you've installed swirl in the past make sure you have version 2.2.21 or later
packageVersion("swirl")

# Every time you want to use swirl, you need to first load the package
library(swirl)

# Install the Getting and Cleaning Data course
install_from_swirl("Getting and Cleaning Data")

# Start swirl and complete the lessons
swirl()


