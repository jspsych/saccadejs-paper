# The recording conditions the validation study's results correlate with error: what each measure
# is, and the code that computes it from the frames the browser recorded. The frame streams are in
# the validation dataset on Zenodo; reading and unpacking them is in R/validation.R.
#
# Most measures come from WebGazer's two 10 x 6 grayscale eye patches, raw (before WebGazer's
# histogram equalization), and from the two eye boxes they were cut from. "Left" is the
# participant's left.
#
# Per participant, over the whole session (recording_conditions, participant_measures):
#   eye brightness           mean pixel value of the left eye's patch
#   left–right brightness    absolute difference between the two patches' mean pixel values
#     difference
#   eye contrast             standard deviation of the left patch's pixels, averaged over frames
#   interocular distance     distance between the eye boxes' centers, as a percentage of the
#                            camera image's width; it stands in for distance from the camera
#   interocular distance SD  its standard deviation over the session: moving nearer and farther
#   head roll                absolute angle of the line from one eye box's center to the other's
#   head roll SD             the roll's (signed) standard deviation over the session
#   head pitch               from MediaPipe's head-pose matrix, as SaccadeJS's headPoseFromMatrix;
#                            positive is the head tipped up
#   eye aspect ratio         openness of each eye, from MediaPipe's landmarks, averaged over eyes
#   blinks                   percentage of frames on which the less open eye's aspect ratio is
#                            below 70% of the participant's median
#   image sharpness          median standard deviation of the 4-neighbour Laplacian of SaccadeJS's
#                            144 x 36 eye image, computed in the browser
#   sensor noise             median, over consecutive frames of a steady fixation, of the mean
#                            absolute change in the patches' pixels
#
# Per frame, against the participant's calibration (frame_conditions, frame_measures). The
# reference is each frame condition averaged over the calibration frames of the first block.
#   head shift               how far the eyes' midpoint moved in the camera image, in
#                            interocular distances at calibration
#   interocular distance     absolute proportional change in interocular distance: moving nearer
#     change                 or farther
#   head yaw, pitch and      absolute change in each angle, in degrees; yaw and pitch from the
#     roll change            head-pose matrix, roll from the eye boxes
#   eye brightness change    absolute change in the left patch's mean pixel value
#   time since calibration   minutes since the last calibration point
#   eye aspect ratio         the less open eye's aspect ratio on that frame
#   blinks                   whether that is below 70% of the participant's median
#   image sharpness          the frame's sharpness, as above

# ------------------------------------------------------------------ per frame

# One frame stream of a trial, as an (n_frames x stride) matrix, or NaN rows if the trial did not
# record it.
frame_stream <- function(row, field, dtype, stride, n_frames) {
  m <- if (is_set(row[[field]])) unpack(row[[field]], dtype, stride) else matrix(NaN, 0, stride)
  if (nrow(m) < n_frames) m <- rbind(m, matrix(NaN, n_frames - nrow(m), stride))
  m[seq_len(n_frames), , drop = FALSE]
}

# The recording conditions on each of a station's scored frames, for correlating with error frame
# by frame. `index` is the scored frames' 0-based positions in the recording. Per frame: the eye
# patches' brightness, left-right difference and contrast (raw, before WebGazer's equalization);
# from the eye boxes, the eyes' midpoint in the camera image, their distance apart and the head's
# roll; yaw and pitch from the head pose; how open the less open eye is; the eye crop's sharpness;
# and the frame-to-frame noise, the mean absolute change in the patches since the previous frame.
frame_conditions <- function(row, left, right, index, n_frames) {
  rows <- index + 1
  eye_boxes <- frame_stream(row, "webgazer_eye_box", "i2", 8, n_frames)[rows, , drop = FALSE]
  eye_aspect <- frame_stream(row, "eye_aspect_ratio", "f2", 2, n_frames)[rows, , drop = FALSE]
  head_pose <- frame_stream(row, "head_pose", "f2", 16, n_frames)[rows, , drop = FALSE]
  image_stats <- frame_stream(row, "eye_image_stats", "f2", 3, n_frames)[rows, , drop = FALSE]
  left_eye <- eye_boxes[, 1:2, drop = FALSE] + eye_boxes[, 3:4, drop = FALSE] / 2
  right_eye <- eye_boxes[, 5:6, drop = FALSE] + eye_boxes[, 7:8, drop = FALSE] / 2
  patches <- cbind(left, right)
  previous <- pmax(rows - 1, 1)
  tibble(
    frame_brightness = rowMeans(left[rows, , drop = FALSE]),
    frame_brightness_difference = abs(rowMeans(left[rows, , drop = FALSE]) -
                                        rowMeans(right[rows, , drop = FALSE])),
    frame_contrast = apply(left[rows, , drop = FALSE], 1, sd_population),
    frame_mid_x = (left_eye[, 1] + right_eye[, 1]) / 2,
    frame_mid_y = (left_eye[, 2] + right_eye[, 2]) / 2,
    frame_iod = row_lengths(left_eye - right_eye),
    frame_roll = atan2(left_eye[, 2] - right_eye[, 2], left_eye[, 1] - right_eye[, 1]) * 180 / pi,
    # As headPoseFromMatrix: yaw = atan2(element (0, 2), element (2, 2)), pitch = asin(-(1, 2)).
    frame_yaw = atan2(head_pose[, 2 * 4 + 0 + 1], head_pose[, 2 * 4 + 2 + 1]) * (180 / pi),
    frame_pitch = asin(pmin(pmax(-head_pose[, 2 * 4 + 1 + 1], -1), 1)) * (180 / pi),
    frame_openness = pmin(eye_aspect[, 1], eye_aspect[, 2]),
    frame_sharpness = image_stats[, 1],
    frame_noise = if_else(rows > 1, rowMeans(abs(patches[rows, , drop = FALSE] - patches[previous, , drop = FALSE])), NA_real_)
  )
}

# ------------------------------------------------------------------ per participant

# One frame stream across every usable capture trial, stacked. A trial missing the stream gets
# NaN rows, so the streams stay aligned frame for frame.
stacked_stream <- function(trials, field, dtype, stride) {
  trials |>
    map(\(row) {
      if (is_set(row[[field]])) unpack(row[[field]], dtype, stride)
      else matrix(NaN, row[["n_frames"]], stride)
    }) |>
    stack_rows()
}

# The mean of the finite values, or NA if there are none.
mean_finite <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_

# Sensor noise, from how much the eye image changes between consecutive frames while the eye
# holds still. For each pair of frames in a station's steady fixation (after the settle period):
# the mean absolute change over WebGazer's 120 patch pixels. For the participant: the median over
# all pairs, so the few with real movement (a blink, a shift of the head) do not count. The
# patches are downsampled, which averages noise away, so this ranks participants rather than
# measuring the camera's noise in gray levels.
frame_to_frame_noise <- function(trials) {
  trials |>
    map(\(row) {
      patches <- cbind(unpack(row[["webgazer_gray_left"]], "u1", PATCH_PIXELS),
                       unpack(row[["webgazer_gray_right"]], "u1", PATCH_PIXELS))
      fixating <- seq_len(nrow(patches)) > (row[["fixation_from"]] %||% 0)
      patches <- patches[fixating, , drop = FALSE]
      if (nrow(patches) < 3) return(NULL)
      rowMeans(abs(diff(patches)))
    }) |>
    list_c() |>
    median()
}

# The participant's recording conditions, averaged over the session. "Left" is the participant's
# left, which is at higher x in the unmirrored camera image.
recording_conditions <- function(trials) {
  trials <- keep(trials, \(row) {
    is_capture_trial(row) && !is_set(row[["practice"]]) && !is_set(row[["page_hidden"]]) &&
      !is_set(row[["window_blurred"]]) && is_set(row[["n_frames"]])
  })
  if (!length(trials)) return(NULL)

  left <- stacked_stream(trials, "webgazer_gray_left", "u1", PATCH_PIXELS)
  right <- stacked_stream(trials, "webgazer_gray_right", "u1", PATCH_PIXELS)
  eye_boxes <- stacked_stream(trials, "webgazer_eye_box", "i2", 8)
  eye_aspect <- stacked_stream(trials, "eye_aspect_ratio", "f2", 2)
  head_pose <- stacked_stream(trials, "head_pose", "f2", 16)
  image_stats <- stacked_stream(trials, "eye_image_stats", "f2", 3)

  # Each eye's box is x, y, width, height; its center is the eye's position. The distance
  # between the eyes stands in for viewing distance, and roll is the angle from the left eye to
  # the right one.
  left_eye <- eye_boxes[, 1:2] + eye_boxes[, 3:4] / 2
  right_eye <- eye_boxes[, 5:6] + eye_boxes[, 7:8] / 2
  interocular <- row_lengths(left_eye - right_eye)
  head_roll <- atan2(left_eye[, 2] - right_eye[, 2], left_eye[, 1] - right_eye[, 1]) * 180 / pi

  # Pitch from the column-major 4 x 4 head-pose matrix, as SaccadeJS's headPoseFromMatrix:
  # R = Ry(yaw) Rx(pitch) Rz(roll), so pitch is asin of minus element (row 1, column 2).
  head_pitch <- asin(pmin(pmax(-head_pose[, 2 * 4 + 1 + 1], -1), 1)) * (180 / pi)

  # Blinks are a relative drop: the less open eye below 70% of this participant's median.
  less_open <- pmin(eye_aspect[, 1], eye_aspect[, 2])
  blinking <- less_open < 0.7 * median(less_open)

  tibble(
    brightness = mean(left),
    asymmetry = mean(left) - mean(right),
    contrast = mean(apply(left, 1, sd_population)),
    iod = mean(interocular),
    iod_sd = sd_population(interocular),
    roll = mean(head_roll),
    roll_sd = sd_population(head_roll),
    pitch = mean_finite(head_pitch),
    ear = mean(eye_aspect),
    closed_pct = 100 * mean(blinking),
    sharpness = median(image_stats[, 1], na.rm = TRUE),
    noise = frame_to_frame_noise(trials),
    # The experiment's own per-frame estimate (Immerkaer, 1996), over the whole eye strip. The
    # strip is mostly edges, which the estimate mistakes for noise, so it tracks sharpness; kept
    # to show that.
    immerkaer_noise = median(image_stats[, 3], na.rm = TRUE)
  )
}

# The participant-level measures as the paper reports them, from `recording_conditions` for every
# session (`observer`) and each session's machine. The difference in brightness between the eyes,
# and the head's roll, count by their size: light from either side, or a tilt toward either
# shoulder, should matter the same. Pitch keeps its sign, since tipping the head down and up are
# not alike: the lids close over the eyes as it goes down. Positive pitch is the head tipped up:
# within a session, pitch fell as targets moved lower on the screen for most participants
# (checked against the per-frame head pose). Interocular distance (and its SD) is a percentage of
# the camera image's width, so it reads independently of the camera's resolution.
participant_measures <- function(observer, machines) {
  observer |>
    left_join(select(machines, session, video_width), by = "session") |>
    mutate(brightness_difference = abs(asymmetry), tilt = abs(roll),
           iod = 100 * iod / video_width, iod_sd = 100 * iod_sd / video_width)
}

# ------------------------------------------------------------------ drift from calibration

# The frame-level measures, added to each scored frame. `frames` holds the per-frame conditions
# from `frame_conditions`, with each frame's time in the session (`elapsed`, ms) and when its
# calibration ended (`cal_end`); `reference` holds each session's calibration reference (see
# replay_session). Drift compares each frame with the participant's calibration frames: how far
# the eyes' midpoint has moved in the image, in interocular distances; how much the interocular
# distance has changed relative to calibration (moving nearer or farther); how much the head has
# turned, tipped and tilted; how much the eye patch's brightness has changed; and the minutes
# since calibrating. Eye state and image: how open the less open eye is, whether it is blinking
# (below 70% of the participant's median), and the eye crop's sharpness. Sensor noise is left
# out: it is a property of the camera, which varies between participants, not within them.
frame_measures <- function(frames, reference) {
  reference <- rename_with(reference, \(n) paste0("cal_", n), -session)
  frames |>
    left_join(reference, by = "session") |>
    mutate(
      `head shift` = sqrt((frame_mid_x - cal_frame_mid_x)^2 + (frame_mid_y - cal_frame_mid_y)^2) / cal_frame_iod,
      `interocular distance change` = abs(frame_iod / cal_frame_iod - 1),
      `head yaw change` = abs(frame_yaw - cal_frame_yaw),
      `head pitch change` = abs(frame_pitch - cal_frame_pitch),
      `head roll change` = abs(frame_roll - cal_frame_roll),
      `time since calibration` = (elapsed - cal_end) / 60000,
      `eye aspect ratio` = frame_openness,
      blinks = as.numeric(frame_openness < 0.7 * median(frame_openness, na.rm = TRUE)),
      `image sharpness` = frame_sharpness,
      `eye brightness change` = abs(frame_brightness - cal_frame_brightness),
      .by = session)
}
