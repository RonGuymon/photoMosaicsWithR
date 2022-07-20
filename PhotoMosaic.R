######## PHOTO MOSAIC ############
# Note:
# A non R way to get images was to use apple's automator that comes with Apple's OS.
# You still have to manually do a google search, but automator is pretty cool

# R libraries----
pacman::p_load(tidyverse, magrittr, lubridate, rvest, xml2, RSelenium, RsimMosaic)

# Read in a list of role models----
# rms <- read.csv('roleModels.csv')
rms <- read.csv('places.csv')

# Use Selenium to get search results----
# Follow instructions from here: https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html
# Download a chrome webdriver that matches the version of chrome that is on your machine: https://chromedriver.chromium.org/downloads
# Unzip the file and save it to the folder where this file is located
# Download the selenium-server-standalone.jar file from here: http://selenium-release.storage.googleapis.com/index.html
# Move the file to the folder where the script is
# Use terminal to navigate to the folder and run the file, or include the full file path: java -jar selenium-server-standalone-x.xx.x.jar
# cd "/Users/rnguymon/Box Sync/Photo Mosaic/" java -jar selenium-server-standalone-4.0.0-alpha-2.jar
startTime <- Sys.time()
remDr <- remoteDriver(remoteServerAddr = 'localhost'
                      , port = 4444L
                      , browserName = 'chrome')

remDr$open()
remDr$navigate('https://images.google.com/')
# remDr$findElement(using = 'name', value = 'q')$highlightElement()
Links <- data.frame()
for(i in 1:nrow(rms)){
  cat('\n', rms$name[i], round(i/nrow(rms)*100), '%')
  tryCatch({
    remDr$findElement(using = 'name', value = 'q')$clearElement()
    remDr$findElement(using = 'name', value = 'q')$sendKeysToElement(list(rms$name[i]))
    remDr$findElement(using = 'name', value = 'q')$sendKeysToElement(list(key = 'enter'))
    Sys.sleep(5)
    thisHtml <- remDr$getPageSource()[[1]]
    html <- xml2::read_html(thisHtml)
    links <- html %>% 
      html_nodes(css = 'img') %>%
      html_attr('src') %>%
      as.data.frame() %>%
      mutate(
        searchTerm = rms$name[i]
      )
    names(links)[1] <- 'url'
    links %<>% filter(!is.na(url))
    Links %<>% bind_rows(links)
  }, error = function(e){
    cat('__Problem__')
  })
}
endTime <- Sys.time()
cat('Duration:', difftime(endTime, startTime))
write_rds(Links, 'Links.rds', compress = 'gz')
# When done, kill the remote driver: kill -9 $(lsof -ti tcp:4444)

# Save the images----
Links <- readRDS('Links.rds')
imgFolder <- './mosaicPhotos'
list.files(imgFolder)
searchTerms <- unique(Links$searchTerm)
for(i in 1:length(searchTerms)){
    if(i %% 10 == 0){
      cat('\n', i, 'Done with', round(i/length(searchTerms), 2))
    }
  tdf <- Links %>% 
    filter(searchTerm == searchTerms[i]) %>%
    filter(grepl('www.gstatic.com/ui/v1/menu', url) == F) %>%
    filter(grepl('^data:image', url) == F)
  picsDownloaded <- 0
  for(j in 1:nrow(tdf)){
    tryCatch({
      fn <- list.files(imgFolder) %>% length()
      
      # All this nullcon stuff is to suppress the message from download.file
      nullcon <- file(nullfile(), open = "wb")
      sink(nullcon, type = "message")
      download.file(tdf$url[j], destfile = paste0(imgFolder, '/img', fn+1, '.jpeg'), quiet = FALSE)
      sink(type = "message")
      close(nullcon)

      picsDownloaded <- picsDownloaded + 1
      if(picsDownloaded == 20){
        break
      }
    }, error = function(e){
      cat('__Problem__')
    })
  }

}

# Convert the images to same sized tiles----
# First, update the createTiles function from RsimMosaic to include a tryCatch statement (see code below)
createTiles2 <- createTiles
fix(createTiles2)
tilesFolder <- './mosaicTiles/' # Make sure to include the forward slash. The function creates the folder.
createTiles2(inPath = imgFolder
             , outPath = tilesFolder
             , tileHeight = 80 # Height of 80 makes pretty clear picture tiles
)

# Create the mosaic----
allTiles <- list.files(tilesFolder)
randomPictureToUse <- paste0(tilesFolder, allTiles[sample(length(allTiles), 1)])
composeMosaicFromImageRandomOptim('CAT797_200.jpg'# Resizing the image to be 200x137 seems like a good balance. 165x305 took several minutes
                                  , 'testMosaic200_places.jpeg'
                                  , tilesFolder)

# function (inPath, outPath, tileHeight = 40, verbose = TRUE)
# {
#   if (verbose) {
#     cat(paste("\n ------------------------------------------------ \n"))
#     cat(paste("    Tiles generation function   \n"))
#     cat(paste(" ------------------------------------------------ \n\n"))
#   }
#   dir.create(file.path(outPath), showWarnings = FALSE)
#   filenameArray <- list.files(inPath, full.names = FALSE)
#   if (verbose) {
#     cat("    -- Creating tiles... this can take a while...\n")
#     cat(paste("    -- Number of tiles that will be created: ",
#               length(filenameArray), "\n", sep = ""))
#   }
#   for (i in 1:length(filenameArray)) {
#     cat('\n', filenameArray[i])
#     tryCatch({
#       img <- jpeg::readJPEG(paste(inPath, "/", filenameArray[i],
#                                   sep = ""))
#       intrpArray <- array(dim = c(tileHeight, tileHeight/dim(img)[1] *
#                                     dim(img)[2], 3))
#       intrpArray[, , 1] <- bilinearInterpolator(img[, , 1],
#                                                 dim(intrpArray)[1], dim(intrpArray)[2])
#       intrpArray[, , 2] <- bilinearInterpolator(img[, , 2],
#                                                 dim(intrpArray)[1], dim(intrpArray)[2])
#       intrpArray[, , 3] <- bilinearInterpolator(img[, , 3],
#                                                 dim(intrpArray)[1], dim(intrpArray)[2])
#       jpeg::writeJPEG(intrpArray[1:tileHeight, 1:tileHeight,
#       ], paste(outPath, filenameArray[i], sep = ""))
#     }, error = function(e){
#       cat('_____Problem')
#     })
#   }
#   if (verbose) {
#     cat(paste("\n"))
#     cat(paste("    Done!\n\n"))
#   }
# }
