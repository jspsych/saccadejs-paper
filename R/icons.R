# Small schematic icons for the recording conditions in the paper's correlation figure.
#
# Two kinds of drawing make up every icon:
#   - an eye image: one eye as the camera sees it, in grayscale, for the conditions measured on
#     the eye image itself (brightness, contrast, sharpness, sensor noise, how open the eye is);
#   - a face: a head seen from the front, for the conditions about where the head is and how it
#     moves (distance from the camera, roll, pitch).
#
# A drawing is a list of parts. A part is a tibble of points with how to draw them: `kind` "fill"
# (a filled polygon), "line" (a path), "dashed" (a dashed path), "dashed_fill" (a polygon with a
# dashed border) or "raster" (a bitmap, between its two corner points), and
# its fill, color, alpha and line width. Drawings are
# made around (0, 0), about 1.1 wide and 0.7 tall, and placed in their row by `place_icon()`. The
# icon panel uses coord_fixed() so circles stay round.

library(dplyr)
library(purrr)
library(tibble)
library(tidyr)

ICON_INK <- "#0b0b0b"
ICON_LINE <- "#52514e"

# ------------------------------------------------------------------ parts

fill_part <- function(x, y, fill, color = NA, alpha = 1, linewidth = 0.25) {
  tibble(x = x, y = y, kind = "fill", fill = fill, color = color, alpha = alpha, linewidth = linewidth)
}

line_part <- function(x, y, color = ICON_INK, alpha = 1, linewidth = 0.35) {
  tibble(x = x, y = y, kind = "line", fill = NA_character_, color = color, alpha = alpha,
         linewidth = linewidth)
}

ellipse_points <- function(cx, cy, width, height, from = 0, to = 2 * pi, n = 48) {
  angle <- seq(from, to, length.out = n)
  list(x = cx + width / 2 * cos(angle), y = cy + height / 2 * sin(angle))
}

ellipse_part <- function(cx, cy, width, height, fill, color = NA, alpha = 1, linewidth = 0.25) {
  p <- ellipse_points(cx, cy, width, height)
  fill_part(p$x, p$y, fill, color, alpha, linewidth)
}

circle_part <- function(cx, cy, radius, fill, color = NA, alpha = 1) {
  ellipse_part(cx, cy, 2 * radius, 2 * radius, fill, color, alpha)
}

rect_part <- function(cx, cy, width, height, fill, color = NA, alpha = 1, linewidth = 0.25) {
  fill_part(cx + c(-1, 1, 1, -1) * width / 2, cy + c(-1, -1, 1, 1) * height / 2,
            fill, color, alpha, linewidth)
}

# Move, scale, rotate or fade every part of a drawing. Rotation is about (cx, cy), in degrees.
shift <- function(drawing, dx = 0, dy = 0) map(drawing, \(p) mutate(p, x = x + dx, y = y + dy))
# Enlarging scales line widths too, so a drawing keeps its proportions at any size.
enlarge <- function(drawing, factor) {
  map(drawing, \(p) mutate(p, x = x * factor, y = y * factor, linewidth = linewidth * factor))
}
rotate <- function(drawing, degrees, cx = 0, cy = 0) {
  a <- degrees * pi / 180
  map(drawing, \(p) {
    mutate(p, x_new = cx + (x - cx) * cos(a) - (y - cy) * sin(a),
              y_new = cy + (x - cx) * sin(a) + (y - cy) * cos(a),
              x = x_new, y = y_new) |>
      select(-x_new, -y_new)
  })
}
fade <- function(drawing, alpha) map(drawing, \(p) mutate(p, alpha = alpha * p$alpha))

# ------------------------------------------------------------------ drawing parts with ggplot

# The ggplot layers for a drawing's parts, one layer per part in drawing order, so later parts
# (the skin over an iris, a lid) sit on top of earlier ones. `parts` is the parts bound into one
# tibble, with a `piece` id per part.
drawing_layers <- function(parts) {
  draw_part <- function(part) {
    switch(part$kind[1],
      fill = geom_polygon(data = part, aes(x, y), fill = part$fill[1], color = part$color[1],
                          alpha = part$alpha[1], linewidth = part$linewidth[1]),
      line = geom_path(data = part, aes(x, y), color = part$color[1], alpha = part$alpha[1],
                       linewidth = part$linewidth[1], lineend = "round"),
      dashed = geom_path(data = part, aes(x, y), color = part$color[1], alpha = part$alpha[1],
                         linewidth = part$linewidth[1], linetype = "22"),
      dashed_fill = geom_polygon(data = part, aes(x, y), fill = part$fill[1], color = part$color[1],
                                 alpha = part$alpha[1], linewidth = part$linewidth[1], linetype = "22"),
      raster = annotation_raster(part$image[[1]], xmin = min(part$x), xmax = max(part$x),
                                 ymin = min(part$y), ymax = max(part$y)))
  }
  parts |> group_split(piece) |> map(draw_part)
}

# A drawing's parts as one tibble, with a `piece` id per part, numbered in drawing order.
bind_parts <- function(drawing, prefix = "part") {
  drawing |>
    imap(\(p, i) mutate(p, piece = sprintf("%s %03d", prefix, i))) |>
    list_rbind()
}

# About how large one drawing unit is in the printed figure, in inches.
INCHES_PER_UNIT <- 0.28

# A drawing rendered to a bitmap and blurred, as one raster part: for an out-of-focus image,
# which polygons cannot show. The bitmap covers `width` x `height` around (0, 0), and `sigma` is
# the blur's spread, in drawing units. It is rendered at the size it is printed, so its line
# widths match the icons drawn as polygons.
blurred_part <- function(drawing, width, height, sigma, pixels_per_unit = 400) {
  bitmap <- render_bitmap(drawing, width, height, pixels_per_unit)
  raster_part(magick::image_blur(bitmap, radius = 0, sigma = sigma * pixels_per_unit), width, height)
}

# The same, but coarsely sampled, `pixels_across` pixels wide: for an image from few camera pixels.
pixelated_part <- function(drawing, width, height, pixels_across, pixels_per_unit = 400) {
  bitmap <- render_bitmap(drawing, width, height, pixels_per_unit)
  info <- magick::image_info(bitmap)
  coarse <- magick::image_scale(bitmap, paste0(pixels_across, "x"))
  raster_part(magick::image_sample(coarse, paste0(info$width, "x", info$height, "!")), width, height)
}

# A drawing as a bitmap covering `width` x `height` around (0, 0).
render_bitmap <- function(drawing, width, height, pixels_per_unit) {
  bitmap <- magick::image_graph(width = round(width * pixels_per_unit),
                                height = round(height * pixels_per_unit), bg = "white",
                                res = pixels_per_unit / INCHES_PER_UNIT)
  print(ggplot() +
          drawing_layers(bind_parts(drawing)) +
          coord_fixed(xlim = c(-width, width) / 2, ylim = c(-height, height) / 2, expand = FALSE) +
          theme_void() +
          theme(plot.margin = margin(0, 0, 0, 0)))
  dev.off()
  bitmap
}

raster_part <- function(bitmap, width, height) {
  tibble(x = c(-width, width) / 2, y = c(-height, height) / 2, kind = "raster",
         image = list(as.raster(bitmap)))
}

# ------------------------------------------------------------------ eye images

# The gray levels of an eye image, from 0 (black) to 1 (white).
EYE_TONES <- c(skin = 0.68, sclera = 0.95, iris = 0.42, pupil = 0.08, lid = 0.25, brow = 0.4)

# Change the tones the way a camera image changes: contrast pulls every tone toward or away from
# the middle gray; brightness moves them all up or down.
# Keep tones between black and white, without losing their names.
clamp_tones <- function(tones) {
  tones[] <- pmin(1, pmax(0, tones))
  tones
}
with_contrast <- function(tones, factor, middle = 0.6) clamp_tones(middle + (tones - middle) * factor)
with_brightness <- function(tones, change) clamp_tones(tones + change)

# The skin patch an eye image sits on, and its border. The border is drawn last, over anything
# that reaches the patch's edge.
skin_patch <- function(tones = EYE_TONES) list(rect_part(0, 0, 1.1, 0.62, fill = gray(tones[["skin"]])))
patch_border <- function() list(rect_part(0, 0, 1.1, 0.62, fill = NA, color = ICON_LINE))

# The shape of the lids: how far each rises above or falls below the eye's midline along its width.
LID_X <- seq(-0.36, 0.36, length.out = 40)
LID_BULGE <- sin(pi * (LID_X + 0.36) / 0.72)

brow <- function(tones) {
  list(line_part(LID_X * 1.05, 0.23 + 0.03 * LID_BULGE, color = gray(tones[["brow"]]), linewidth = 0.5))
}

# One open eye, as the camera sees it: an almond-shaped opening in the skin, with the white of the
# eye, the iris and pupil, a small highlight, the lids and the brow. `opening` scales how open the
# eye is (1 is a relaxed, open eye). The iris is drawn whole and then the skin above and below the
# lids is painted over it, so a narrowing eye hides the iris as a real one does.
eye_image <- function(tones = EYE_TONES, opening = 1, background = TRUE) {
  tone <- \(name) gray(tones[[name]])
  upper <- 0.19 * opening * LID_BULGE + 0.02
  lower <- -0.12 * opening * LID_BULGE + 0.02
  eye <- list(
    fill_part(c(LID_X, rev(LID_X)), c(upper, rev(lower)), fill = tone("sclera")),
    circle_part(0, 0.03, 0.13, fill = tone("iris")),
    circle_part(0, 0.03, 0.06, fill = tone("pupil")),
    circle_part(0.04, 0.07, 0.022, fill = "white")
  )
  # The skin above the upper lid and below the lower one, painted over the iris.
  lid_masks <- list(
    fill_part(c(LID_X, rev(LID_X)), c(upper, rep(0.31, length(LID_X))), fill = tone("skin")),
    fill_part(c(LID_X, rev(LID_X)), c(lower, rep(-0.31, length(LID_X))), fill = tone("skin"))
  )
  lids <- list(line_part(LID_X, upper, color = tone("lid"), linewidth = 0.6),
               line_part(LID_X, lower, color = tone("lid"), linewidth = 0.25))
  features <- c(eye, lid_masks, lids, brow(tones))
  if (background) c(skin_patch(tones), features, patch_border()) else features
}

# A closed eye: the lid's downward curve, with lashes, and the brow.
closed_eye_image <- function(tones = EYE_TONES) {
  lid_color <- gray(tones[["lid"]])
  lid <- 0.02 - 0.05 * LID_BULGE
  lash_x <- seq(-0.2, 0.2, length.out = 5)
  lash_y <- 0.02 - 0.05 * sin(pi * (lash_x + 0.36) / 0.72)
  lashes <- map2(lash_x, lash_y, \(x, y) line_part(c(x, x * 1.15), c(y, y - 0.07), color = lid_color,
                                                   linewidth = 0.4))
  c(skin_patch(tones), list(line_part(LID_X, lid, color = lid_color, linewidth = 0.6)), lashes,
    brow(tones), patch_border())
}

# An out-of-focus eye: the eye image, really blurred, inside a crisp patch border.
blurred_eye_image <- function() {
  c(list(blurred_part(c(skin_patch(), eye_image(background = FALSE)), 1.1, 0.62, sigma = 0.016)),
    patch_border())
}

# An eye from few camera pixels: the eye image, coarsely sampled, inside a crisp patch border.
pixelated_eye_image <- function(pixels_across = 22) {
  c(list(pixelated_part(c(skin_patch(), eye_image(background = FALSE)), 1.1, 0.62, pixels_across)),
    patch_border())
}

# An eye with sensor noise: speckles of random gray over the image.
noisy_eye_image <- function() {
  set.seed(4)
  speckles <- tibble(cx = runif(90, -0.52, 0.52), cy = runif(90, -0.28, 0.28), level = runif(90, 0.1, 1))
  c(skin_patch(), eye_image(background = FALSE),
    pmap(speckles, \(cx, cy, level) rect_part(cx, cy, 0.04, 0.04, fill = gray(level), alpha = 0.75)),
    patch_border())
}

# Both eyes, small, as the camera sees them: evenly lit, or with one darker than the other, as
# when light comes from one side.
eye_pair_image <- function(difference = 0) {
  c(eye_image() |> enlarge(0.5) |> shift(-0.275, 0),
    eye_image(with_brightness(EYE_TONES, -difference)) |> enlarge(0.5) |> shift(0.275, 0))
}

# ------------------------------------------------------------------ faces

# A head seen from the front: ears, face, hair, brows, eyes, nose and mouth. It tilts about the
# neck, below the face, as a head does. `nod` tips it down (negative) or up (positive): seen from
# the front, the features move down or up within the face, showing more of the top of the head
# or more of the chin.
face_drawing <- function(scale = 1, degrees = 0, nod = 0, turn = 0) {
  rise <- 0.08 * nod
  # Turning (yaw) slides the features sideways within the face.
  slide <- 0.085 * turn
  eye <- function(cx) list(ellipse_part(cx + slide, 0.03 + rise, 0.06, 0.04, fill = ICON_INK))
  # The hair: the top of the head down to a curved hairline.
  hairline_y <- 0.12 + rise
  crown <- ellipse_points(0, 0, 0.47, 0.61, from = 0, to = pi)
  crown_above <- crown$y >= hairline_y
  hairline <- ellipse_points(0, hairline_y, 0.4, 0.2, from = pi, to = 0)
  hairline_above <- hairline$y >= hairline_y
  mouth <- ellipse_points(0, -0.13 + rise, 0.14, 0.07, from = pi * 1.15, to = pi * 1.85)

  c(
    list(ellipse_part(-0.24, rise / 2, 0.07, 0.13, fill = "white", color = ICON_INK),
         ellipse_part(0.24, rise / 2, 0.07, 0.13, fill = "white", color = ICON_INK),
         ellipse_part(0, 0, 0.46, 0.6, fill = "white", color = ICON_INK, linewidth = 0.35),
         fill_part(c(crown$x[crown_above], hairline$x[hairline_above]),
                   c(crown$y[crown_above], hairline$y[hairline_above]), fill = "gray30"),
         line_part(c(-0.15, -0.07) + slide, c(0.09, 0.1) + rise, linewidth = 0.3),
         line_part(c(0.07, 0.15) + slide, c(0.1, 0.09) + rise, linewidth = 0.3)),
    eye(-0.11), eye(0.11),
    list(line_part(c(0, -0.025, 0.02) + slide * 1.3, c(0, -0.07, -0.08) + rise, linewidth = 0.3),
         line_part(mouth$x + slide, mouth$y, linewidth = 0.35))
  ) |>
    enlarge(scale) |>
    rotate(degrees, cx = 0, cy = -0.4 * scale)
}

# Two faces side by side: the same head at two moments, for how much it moved over the session.
# `scales` sets each face's size (nearer or farther from the camera), `degrees` each one's tilt,
# and `dx` and `dy` where each one sits.
face_pair <- function(scales = c(1, 1), degrees = c(0, 0), dx = c(-0.27, 0.27), dy = c(0, 0)) {
  base <- 0.68
  c(face_drawing(scale = base * scales[1], degrees = degrees[1]) |> shift(dx[1], dy[1]),
    face_drawing(scale = base * scales[2], degrees = degrees[2]) |> shift(dx[2], dy[2]))
}

# Where the head was during calibration: a faint, dashed outline. Drift icons draw it behind the
# current head, so a head that has not moved hides it, and one that has leaves it showing.
calibration_outline <- function() {
  outline <- ellipse_points(0, 0, 0.46, 0.6)
  list(line_part(outline$x, outline$y, color = ICON_LINE, alpha = 0.7, linewidth = 0.35) |>
         mutate(kind = "dashed"))
}

# The current head over the calibration outline. Low: the head is where it was.
drifted_face <- function(...) c(calibration_outline(), face_drawing(...))

# A clock: how long since calibrating, as the sector its hand has swept.
clock <- function(minutes_fraction) {
  rim <- ellipse_points(0, 0, 0.56, 0.56)
  ticks <- map(seq(0, 2 * pi, length.out = 13)[-13], \(a) {
    line_part(0.24 * c(sin(a), sin(a) * 1.15), 0.24 * c(cos(a), cos(a) * 1.15), linewidth = 0.3)
  })
  sweep <- if (minutes_fraction > 0) {
    a <- seq(0, 2 * pi * minutes_fraction, length.out = 40)
    list(fill_part(c(0, 0.24 * sin(a)), c(0, 0.24 * cos(a)), fill = "#d6d5ce"))
  }
  hand_angle <- 2 * pi * minutes_fraction
  c(list(fill_part(rim$x, rim$y, fill = "white", color = ICON_INK, linewidth = 0.4)), sweep, ticks,
    list(line_part(c(0, 0.2 * sin(hand_angle)), c(0, 0.2 * cos(hand_angle)), linewidth = 0.6),
         circle_part(0, 0, 0.02, fill = ICON_INK)))
}

# A strip of film with `n` frames: how many frames the camera records each second.
film_strip <- function(n) {
  gap <- 0.03
  frame_width <- (1 - gap * (n + 1)) / n
  frames <- map(seq_len(n), \(i) {
    rect_part(-0.5 + gap + (i - 0.5) * frame_width + (i - 1) * gap, 0, frame_width, 0.2,
              fill = "white")
  })
  c(list(rect_part(0, 0, 1, 0.3, fill = ICON_INK)), frames)
}

# A meter: a bar filled to `level`, from 0 to 1.
meter <- function(level) {
  list(rect_part(0, 0, 0.9, 0.22, fill = "white", color = ICON_INK, linewidth = 0.35),
       rect_part(-0.45 + 0.45 * level, 0, 0.9 * level, 0.22, fill = ICON_LINE))
}

# An eye now, in front of the same eye during calibration, drawn behind it as a ghost with a
# dashed border, as the drift icons draw the calibration head. Low: the same brightness as then.
brightness_since_calibration <- function(darker = 0) {
  then <- eye_image() |>
    map(\(p) if (identical(p$color[1], ICON_LINE)) mutate(p, kind = "dashed_fill") else p) |>
    enlarge(0.85) |>
    shift(-0.1, 0.08)
  now <- eye_image(with_brightness(EYE_TONES, -darker)) |> enlarge(0.85) |> shift(0.06, -0.05)
  c(then, now)
}

# ------------------------------------------------------------------ one icon per condition

# Each condition's icon, as a drawing of a low value and of a high one.
condition_icon <- function(covariate) {
  switch(covariate,
    "eye contrast" = list(low = eye_image(with_contrast(EYE_TONES, 0.3)),
                          high = eye_image(with_contrast(EYE_TONES, 1.3))),
    "eye brightness" = list(low = eye_image(with_brightness(EYE_TONES, -0.4)),
                            high = eye_image(with_brightness(EYE_TONES, 0.2))),
    # How different the two eyes' brightness is, whichever is darker.
    "left–right brightness difference" = list(low = eye_pair_image(0), high = eye_pair_image(0.35)),
    "image sharpness" = list(low = blurred_eye_image(), high = eye_image()),
    "sensor noise" = list(low = eye_image(), high = noisy_eye_image()),
    "eye aspect ratio" = list(low = eye_image(opening = 0.4), high = eye_image(opening = 1.15)),
    "blinks" = list(low = eye_image(), high = closed_eye_image()),
    # The distance between the eyes in the camera image: a face far from the camera, and near.
    "interocular distance" = list(low = face_drawing(scale = 0.75), high = face_drawing(scale = 1.2)),
    "interocular distance SD" = list(low = face_pair(scales = c(0.95, 1.02)),
                                     high = face_pair(scales = c(0.7, 1.3))),
    # How far the head tilted from upright, either way.
    "head roll" = list(low = face_drawing(), high = face_drawing(degrees = 20)),
    # Each head tilts away from the other (positive degrees tilt it to the image's left).
    "head roll SD" = list(low = face_pair(degrees = c(4, -4)), high = face_pair(degrees = c(25, -25))),
    # Positive pitch is the head tipped up (see the pitch check in the paper's covariates chunk).
    "head pitch" = list(low = face_drawing(nod = -1), high = face_drawing(nod = 1)),
    # How far the head turned from facing the camera, either way.
    "head yaw" = list(low = face_drawing(), high = face_drawing(turn = 1)),
    # How much the head moved over the session, side to side or up and down: the same head at two
    # moments, close together or far apart.
    "head position SD, left–right" = list(low = face_pair(dx = c(-0.2, 0.2)),
                                          high = face_pair(dx = c(-0.38, 0.38))),
    "head position SD, up–down" = list(low = face_pair(dy = c(0.02, -0.02)),
                                       high = face_pair(dy = c(0.13, -0.13))),
    # Within participants: what changed since calibration, frame by frame.
    "head shift" = list(low = drifted_face(), high = c(calibration_outline(), face_drawing() |> shift(0.2, 0.05))),
    # A size either way; drawn as moving away, so the calibration outline shows around the head.
    "interocular distance change" = list(low = drifted_face(), high = drifted_face(scale = 0.75)),
    "head yaw change" = list(low = drifted_face(), high = drifted_face(turn = 1)),
    "head pitch change" = list(low = drifted_face(), high = drifted_face(nod = -1)),
    "head roll change" = list(low = drifted_face(), high = drifted_face(degrees = 20)),
    "time since calibration" = list(low = clock(0), high = clock(0.35)),
    "eye brightness change" = list(low = brightness_since_calibration(0),
                                   high = brightness_since_calibration(0.35)),
    "model quality weight" = list(low = meter(0.25), high = meter(0.9)),
    "model calibration weight" = list(low = meter(0.25), high = meter(0.9)),
    "camera frame rate" = list(low = film_strip(3), high = film_strip(8)),
    # How many camera pixels the eye crop spans.
    "eye crop width" = list(low = pixelated_eye_image(), high = eye_image()),
    stop("No icon for condition: ", covariate))
}

# Where the parts of a row's icon go, horizontally: low and high either side of an arrow.
ICON_LOW_X <- 0.6
ICON_ARROW_X <- 1.6
ICON_HIGH_X <- 2.6
ICON_WIDTH <- 3.2

# A drawing at (dx, dy), as one tibble with an id per part, numbered in drawing order.
place_drawing <- function(drawing, dx, dy, prefix) drawing |> shift(dx, dy) |> bind_parts(prefix)

# One row's icon, at height `row`: its parts, and the arrow from low to high.
place_icon <- function(covariate, row) {
  icon <- condition_icon(covariate)
  list(parts = bind_rows(place_drawing(icon$low, ICON_LOW_X, row, paste(covariate, "a low")),
                         place_drawing(icon$high, ICON_HIGH_X, row, paste(covariate, "b high"))),
       arrow = tibble(x = ICON_ARROW_X - 0.25, y = row, xend = ICON_ARROW_X + 0.25, yend = row))
}

# The icon panel: one icon per condition, at the rows given (1 at the bottom). `n_rows` is how
# many rows the matching plot has, if some (headings, say) have no icon.
icon_panel <- function(covariates, rows, n_rows = max(rows)) {
  icons <- map2(covariates, rows, place_icon)
  ggplot() +
    drawing_layers(map(icons, "parts") |> list_rbind()) +
    geom_segment(data = map(icons, "arrow") |> list_rbind(), aes(x, y, xend = xend, yend = yend),
                 color = ICON_LINE, linewidth = 0.3, arrow = arrow(length = unit(0.035, "in"))) +
    coord_fixed(xlim = c(0, ICON_WIDTH), ylim = c(0.5, n_rows + 0.5), expand = FALSE) +
    theme_void()
}

# ------------------------------------------------------------------ calibration grids

# A strip of calibration-grid icons to sit under a plot with one discrete x position per grid:
# each grid's points (`positions`, a list of two-column matrices of screen percents) inside the
# outline of a 16:9 screen, centered under its x position. `width` and `height` are the screen's
# size in x positions and in the strip's own units; `labels` go under the icons as the x axis.
# The points are drawn `inset` of the way in from the screen's edges, so points at the edge stay
# clear of the outline.
grid_icon_strip <- function(positions, labels, width = 0.78, height = 1, inset = 0.1) {
  screens <- tibble(i = seq_along(positions))
  spread <- 1 - 2 * inset
  points <- imap(positions, \(m, i) tibble(x = i + (m[, 1] / 100 - 0.5) * spread * width,
                                           y = (0.5 - m[, 2] / 100) * spread * height)) |>
    list_rbind()
  ggplot() +
    geom_rect(data = screens, aes(xmin = i - width / 2, xmax = i + width / 2,
                                  ymin = -height / 2, ymax = height / 2),
              fill = "white", color = ICON_LINE, linewidth = 0.3) +
    geom_point(data = points, aes(x, y), size = 0.35, stroke = 0, color = ICON_INK) +
    scale_x_continuous(breaks = seq_along(positions), labels = labels) +
    # A discrete axis's range, 0.6 beyond the first and last positions, so the icons line up with
    # the plot above (which needs coord_cartesian(xlim = c(1, n)) to ignore its own jitter).
    coord_cartesian(xlim = c(0.4, length(positions) + 0.6), ylim = c(-height, height) * 0.56,
                    expand = FALSE) +
    theme(panel.grid = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
          axis.ticks = element_blank(), axis.line = element_blank())
}
