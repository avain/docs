print_data_stats <- function(data) {
  cat("Overhead stats\n")
  cat("Median: ", median(data$Overhead, na.rm = TRUE), "\n")
  cat("Mean: ", mean(data$Overhead, na.rm = TRUE), "\n")
  cat("Min: ", min(data$Overhead, na.rm = TRUE), "\n")
  cat("Max: ", max(data$Overhead, na.rm = TRUE), "\n")
}

plot_size_overhead <- function(data) {
  colors <- c("black","grey")
  
  # Adjust plotting params (mar=margin size)
  par(mar=c(7,9,2, 3), mai=c(1,2,1,1))
  
  barplot(
    rbind(data$InputBinSize / 1024, data$RevgenBinSize / 1024), 
    horiz=TRUE, 
    names.arg = data$BinaryName, 
    las=1, 
    xlab = "Binary size (KB)",
    xlim = c(1, 10000),
    log = "x",
    col=colors,
    yaxs = "i" # This removes the 4% margin around the plot
  )
  
  legend_labels <- c(
    "Input binary", 
    "Output binary"
  )
  
  title("Binary Sizes (KB)", line=3)
  legend("topright", legend_labels, fill = colors)
  grid()
  axis(side = 3)
}

data <- read.delim('cgc-binaries.stats', na.strings = "N/A")

# Clear broken data
data[is.na(data$RevgenBinSize),]$InputBinSize <- 1024

# Compute overhead
data['Overhead'] <- data$RevgenBinSize / data$InputBinSize

# na <- na.omit(data)
old.par <- par(mar = c(0, 0, 0, 0))

svg("cgc-binaries.svg", width = 10, height=60 )

plot_size_overhead(data)

dev.off()

print_data_stats(data)