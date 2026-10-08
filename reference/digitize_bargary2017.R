# Digitize Bargary et al. (2017) Fig. 1, "Pro-saccade latency": the distribution of participants'
# median pro-saccade latency.
#
# Bargary, G., Bosten, J. M., Goodbourn, P. T., Lawrance-Owen, A. J., Hogg, R. E., & Mollon, J. D.
# (2017). Individual differences in human eye movements: An oculomotor signature? Vision Research,
# 141, 157-169. https://vision.psychol.cam.ac.uk/jdmollon/papers/Pergenic%20study%20of%20the%20oculomotor%20signature%202017.pdf
#
# Fig. 1 is a scanned image on page 6 of the PDF. To rerun, extract it beside this script:
#   pdfimages -f 6 -l 6 -png <paper.pdf> bfig      (writes bfig-000.png)
# then `Rscript digitize_bargary2017.R`. The x axis is calibrated from the tick marks (which match
# the label centers) and the y axis from the frame (0 to 200 participants). The bins are the 38
# equal bins over the range the paper reports (142-322 ms), which match the bars' edges to about
# half a millisecond. The digitized distribution reproduces the paper's Table 1: N 1031 (of about
# 1058; one-participant bins in the far tail are too short to register), mean 178.0 ms (177.2),
# median 174.5 (174), SD 19.0 (18.5). The bin at 300 ms is the axis tick mark and was set to 0 in
# the saved file.
library(magick)
img <- image_read("bfig-000.png")
crop <- image_crop(img, "560x260+560+20")
m <- as.integer(image_data(crop, "gray"))[, , 1]   # [y, x], 0 = black
dark <- m < 110

# The frame: the rows and columns that are dark across most of the panel.
row_dark <- rowSums(dark); col_dark <- colSums(dark)
frame_rows <- which(row_dark > 0.6 * ncol(m))
frame_cols <- which(col_dark > 0.6 * (max(frame_rows) - min(frame_rows)))
top <- min(frame_rows); bottom <- max(frame_rows)
left <- min(frame_cols); right <- max(frame_cols)
cat("frame rows", range(frame_rows), " cols", range(frame_cols), "\n")

# Ticks hang down from the top frame line; find their columns just below it.
below_top <- dark[top + 3, (left + 3):(right - 3)]
tick_cols <- which(below_top) + left + 2
tick_groups <- split(tick_cols, cumsum(c(1, diff(tick_cols) > 1)))
tick_x <- sapply(tick_groups, mean)
cat("tick x:", round(tick_x, 1), "\n")
# Cross-check: the centers of the x-axis labels, below the frame.
label_band <- dark[(bottom + 6):(bottom + 45), ]
label_cols <- which(colSums(label_band) > 0)
label_groups <- split(label_cols, cumsum(c(1, diff(label_cols) > 12)))
cat("label centers:", round(sapply(label_groups, \(g) (min(g) + max(g)) / 2), 1), "\n")
# Frame line thickness, to find its inner edges.
cat("top rows dark:", which(row_dark[top:(top + 6)] > 0.6 * ncol(m)) + top - 1, "\n")
cat("bottom rows dark:", which(row_dark[(bottom - 6):bottom] > 0.6 * ncol(m)) + bottom - 7, "\n")
inner_top <- 37; inner_bottom <- 168
px_per_ms <- (tick_x[4] - tick_x[1]) / 150
ms_at <- function(x) 150 + (x - tick_x[1]) / px_per_ms
count_per_px <- 200 / (inner_bottom - inner_top + 1)
# Each column's bar height: the dark run up from the bottom inner edge.
heights <- sapply((left + 4):(right - 4), \(x) {
  col <- dark[inner_top:inner_bottom, x]
  run <- rle(rev(col))
  if (run$values[1]) run$lengths[1] else 0
})
cols <- (left + 4):(right - 4)
d <- data.frame(x = cols, ms = ms_at(cols), height = heights)
d <- d[d$height > 0, ]
# Bars: runs of columns with the same height (allowing a pixel of anti-aliasing).
change <- c(TRUE, abs(diff(d$height)) > 1 | diff(d$x) > 1)
d$bar <- cumsum(change)
bars <- aggregate(cbind(x, height) ~ bar, d, \(v) c(min = min(v), max = max(v), med = median(v)))
bars <- data.frame(from_ms = ms_at(bars$x[, "min"] - 0.5), to_ms = ms_at(bars$x[, "max"] + 0.5),
                   count = round(bars$height[, "med"] * count_per_px))
print(bars, row.names = FALSE)
# The figure's bins: 38 equal bins over the reported range, 142-322 ms (Table 1).
edges <- seq(142, 322, length.out = 39)
centers <- (head(edges, -1) + tail(edges, -1)) / 2
x_at <- function(ms) tick_x[1] + (ms - 150) * px_per_ms
bin_height <- sapply(centers, \(ms) {
  x <- round(x_at(ms))
  col <- dark[inner_top:inner_bottom, x]
  run <- rle(rev(col))
  if (run$values[1]) run$lengths[1] else 0
})
hist_bargary <- data.frame(from_ms = head(edges, -1), to_ms = tail(edges, -1), center_ms = centers,
                           count = round(bin_height * count_per_px))
print(hist_bargary[hist_bargary$count > 0, ], row.names = FALSE)
n <- sum(hist_bargary$count)
mean_ms <- sum(hist_bargary$count * hist_bargary$center_ms) / n
cum <- cumsum(hist_bargary$count) / n
k <- which(cum >= 0.5)[1]
median_ms <- hist_bargary$from_ms[k] + (0.5 - c(0, cum)[k]) / (cum[k] - c(0, cum)[k]) * (edges[2] - edges[1])
sd_ms <- sqrt(sum(hist_bargary$count * (hist_bargary$center_ms - mean_ms)^2) / n)
cat(sprintf("N %d (paper: ~1058)  mean %.1f (paper 177.2)  median %.1f (paper 174)  SD %.1f (paper 18.5)\n",
            n, mean_ms, median_ms, sd_ms))
write.csv(hist_bargary, "bargary2017_prosaccade_latency.csv", row.names = FALSE)
