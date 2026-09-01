###############################################################
################### PERSONAL FRAME GENERATOR ##################
###############################################################

# Install once if needed:
# install.packages("magick")

library(magick)


###############################################################
################ PARTICIPANT SELECTION ########################
###############################################################

# Select the participant's FOUR chosen pictures.
# Category names MUST match the folder names exactly.

photos <- data.frame(

  Category = c(
    "Albums",
    "Books",
    "Cities",
    "Hobbies"
  ),

  Number = c(
    1,
    2,
    12,
    7
  ),

  stringsAsFactors = FALSE

)

# Participant's chosen colour (see Colours.csv)

participant_colour <- 5


###############################################################
#################### FOLDER STRUCTURE ##########################
###############################################################

picture_folder <- "Pictures"
icon_folder <- "Icons"
output_folder <- "Output"

colour_file <- "Colours.csv"

like_icon_file <- file.path(
  icon_folder,
  "user_like.png"
)

no_like_icon_file <- file.path(
  icon_folder,
  "no_like.png"
)


###############################################################
################### READ COLOUR TABLE ##########################
###############################################################

colour_table <- read.csv(colour_file)

border_colour <- colour_table$Hex[
  colour_table$ID == participant_colour
]

if(length(border_colour) == 0){

  stop("Colour ID not found.")

}


###############################################################
################### READ PARTICIPANT PHOTOS ####################
###############################################################

read_photo <- function(category, number){

  filename <- sprintf(
    "%03d.png",
    number
  )

  filepath <- file.path(
    picture_folder,
    category,
    filename
  )

  if(!file.exists(filepath)){

    stop(
      paste(
        "Cannot find:",
        filepath
      )
    )

  }

  image_read(filepath)

}


images <- vector(
  "list",
  4
)

for(i in 1:4){

  images[[i]] <- read_photo(
    photos$Category[i],
    photos$Number[i]
  )

}


###############################################################
###################### RESIZE IMAGES ###########################
###############################################################

# Crop each selected image into a square
# without stretching/squeezing it.

images <- lapply(
  images,
  function(img){

    img %>%
      image_resize("410x410^") %>%
      image_crop(
        "410x410+0+0",
        gravity = "center"
      )

  }
)


###############################################################
##################### BUILD 2x2 GRID ###########################
###############################################################

top <- image_append(
  c(
    images[[1]],
    images[[2]]
  )
)

bottom <- image_append(
  c(
    images[[3]],
    images[[4]]
  )
)

frame <- image_append(
  c(
    top,
    bottom
  ),
  stack = TRUE
)


###############################################################
############### PARTICIPANT COLOUR BORDER ######################
###############################################################

frame <- image_border(
  frame,
  color = border_colour,
  geometry = "45x45"
)


###############################################################
##################### WHITE OUTER BORDER #######################
###############################################################

frame <- image_border(
  frame,
  color = "white",
  geometry = "15x15"
)


###############################################################
###################### SAVE PREVIEW ############################
###############################################################

preview <- image_resize(
  frame,
  "450x450!"
)

image_write(
  preview,
  path = file.path(
    output_folder,
    "preview.png"
  ),
  format = "png"
)


###############################################################
#################### CHECK NOTIFICATION IMAGES #################
###############################################################

if(!file.exists(like_icon_file)){

  stop(
    "Cannot find Icons/user_like.png"
  )

}

if(!file.exists(no_like_icon_file)){

  stop(
    "Cannot find Icons/no_like.png"
  )

}


###############################################################
################### LOAD NOTIFICATION BARS #####################
###############################################################

# Keep the notification bars at exactly 320 x 97 pixels.

like_bar <- image_read(
  like_icon_file
)

like_bar <- image_resize(
  like_bar,
  "320x97!"
)


no_like_bar <- image_read(
  no_like_icon_file
)

no_like_bar <- image_resize(
  no_like_bar,
  "320x97!"
)


###############################################################
#################### USERNAME LIST #############################
###############################################################

# 40 usernames for the experimental trials.

trial_usernames <- c(

  "@bunny123",
  "@themadgui79",
  "@inesinthecity",
  "@joaopm_92",
  "@sunnydays47",
  "@rita.lopes",
  "@catlover_22",
  "@miguelzinho",
  "@cloudy_maria",
  "@andrews_world",
  "@beatriz.jpg",
  "@pedro_silva7",
  "@littlerainbow",
  "@sofiacosta",
  "@martim_04",
  "@justanotherday",
  "@catarina_m",
  "@joaopereira_",
  "@moonchild88",
  "@inesmendes",
  "@thequietone",
  "@mariadias_",
  "@tommyonthego",
  "@beatrizvibes",
  "@ricardo_91",
  "@pastelclouds",
  "@sofiaa_21",
  "@diogomartins",
  "@happylittlebean",
  "@carolinam_",
  "@joaoribeiro8",
  "@sunflowergirl",
  "@mariana_s",
  "@tiagoferreira",
  "@midnightcoffee",
  "@inespires",
  "@littlefox_17",
  "@matilde_rosa",
  "@pedro.costa",
  "@wanderlust_jo"

)


# 3 usernames for the TRAINING trials.

training_usernames <- c(

  "@_ricky_67",
  "@sunny_side_up",
  "@tiagom_90"

)


###############################################################
############### CREATE TEXT NOTIFICATION BAR ##################
###############################################################

make_notification <- function(
  username
){

  # Start with the existing notification bar.

  notification <- like_bar


  # Add username + Portuguese message.

  notification <- image_annotate(

    notification,

    text = paste0(
      username,
      "\n",
      "gostou das tuas fotos!"
    ),

    font = "Arial-Bold",

    size = 23,

    color = "white",

    gravity = "west",

    location = "+72-15",

  )


  return(notification)

}


###############################################################
################ CREATE TRIAL FEEDBACK IMAGES ##################
###############################################################

for(i in seq_along(trial_usernames)){

  # Create notification containing username.

  notification <- make_notification(
    trial_usernames[i]
  )


  # Place notification in the centre
  # of the participant's personal frame.

  feedback <- image_composite(
    frame,
    notification,
    gravity = "center"
  )


  # Add thin green feedback border.

  feedback <- image_border(
    feedback,
    color = "#00FF00",
    geometry = "8x8"
  )


  # Resize final image for E-Prime.

  feedback <- image_resize(
    feedback,
    "450x450!"
  )


  # Save.

  image_write(
    feedback,
    path = file.path(
      output_folder,
      paste0(
        "like_green_",
        i,
        ".png"
      )
    ),
    format = "png"
  )

}


###############################################################
############### CREATE TRAINING FEEDBACK IMAGES ###############
###############################################################

for(i in seq_along(training_usernames)){

  # Create notification containing username.

  notification <- make_notification(
    training_usernames[i]
  )


  # Place notification in the centre
  # of the participant's personal frame.

  feedback <- image_composite(
    frame,
    notification,
    gravity = "center"
  )


  # Add thin green feedback border.

  feedback <- image_border(
    feedback,
    color = "#00FF00",
    geometry = "8x8"
  )


  # Resize final image for E-Prime.

  feedback <- image_resize(
    feedback,
    "450x450!"
  )


  # Save.

  image_write(
    feedback,
    path = file.path(
      output_folder,
      paste0(
        "like_green_training_",
        i,
        ".png"
      )
    ),
    format = "png"
  )

}


###############################################################
################### CREATE NO-LIKE GREEN #######################
###############################################################

# Neutral + Hit

nolike_green <- image_composite(
  frame,
  no_like_bar,
  gravity = "center"
)

nolike_green <- image_border(
  nolike_green,
  color = "#00FF00",
  geometry = "8x8"
)

nolike_green <- image_resize(
  nolike_green,
  "450x450!"
)

image_write(
  nolike_green,
  path = file.path(
    output_folder,
    "nolike_green.png"
  ),
  format = "png"
)


###############################################################
##################### CREATE RED FEEDBACK ######################
###############################################################

# Gain/Neutral + Miss

nolike_red <- image_border(
  frame,
  color = "#FF0100",
  geometry = "8x8"
)

nolike_red <- image_resize(
  nolike_red,
  "450x450!"
)

image_write(
  nolike_red,
  path = file.path(
    output_folder,
    "nolike_red.png"
  ),
  format = "png"
)


###############################################################
######################## FINISHED ##############################
###############################################################

cat("\n")
cat("========================================\n")
cat(" PERSONAL FRAME CREATED SUCCESSFULLY!\n")
cat("========================================\n")
cat("\n")

cat("Preview:\n")
cat("  preview.png\n\n")

cat("Standard feedback:\n")
cat("  nolike_green.png\n")
cat("  nolike_red.png\n\n")

cat("Trial username feedback:\n")
cat("  like_green_1.png through like_green_40.png\n\n")

cat("Training username feedback:\n")
cat("  like_green_training_1.png through like_green_training_3.png\n\n")

cat("Saved to:\n")
cat(output_folder)
cat("\n")
cat("========================================\n")