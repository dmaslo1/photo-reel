# 1. LOAD PACKAGES ----
library(shiny)
library(shinyMobile)
library(shinyjs)

# 2. CONFIGURATION ----
# !!! IMPORTANT: Change this to your NAS DDNS address and port !!!
# For local testing, you can use: "http://192.168.0.50/"
nas_base_url <- "http://192.168.0.50/my_app_photos/"



# 3. USER INTERFACE (UI) ----
ui <- f7Page(
  title = "My Photo Feed",
  options = list(
    theme = "auto",
    dark = TRUE,
    pullToRefresh = TRUE # Allows user to pull down to refresh the app
  ),
  # Use shinyjs to enable JavaScript interactions
  useShinyjs(), 
  
  # Main layout of the app
  f7SingleLayout(
    navbar = f7Navbar(
      title = "Photo Feed",
      hairline = TRUE,
      shadow = TRUE
    ),
    # Main content area
    div(
      # Add some padding around the content
      class = "page-content",
      style = "padding-top: 5px; padding-bottom: 20px;",
      
      # Centered Shuffle Button
      f7Block(
        f7Button("shuffle_button", "Shuffle Photos", fill = TRUE, rounded = TRUE)
      ),
      
      # This is where our feed of photos will be dynamically inserted
      uiOutput("photo_feed"),
      
      # A hidden element to detect when we scroll to the bottom
      div(id = "scroll_trigger", style = "height: 10px;"),
      
      # Loading spinner that shows while more images are loaded
      f7Block(
        # f7Preloader(color = "white"),``
        p("Loading more...", style = "text-align: center; color: #888;")
      )
    )
  )
)

# 4. SERVER LOGIC ----
server <- function(input, output, session) {
  
  # --- Reactive Values to store state ---
  # A list to hold the full, shuffled list of photo URLs
  shuffled_photo_urls <- reactiveVal(character(0))
  # A counter for how many photos are currently displayed on screen
  photos_to_display_count <- reactiveVal(10) # Start by showing 10 photos
  
  # --- Initial Data Loading ---
  # This observer runs once when the app starts
  observe({
    file_list_url <- paste0(nas_base_url, "file_list.txt")
    
    tryCatch({
      # URL encoding spaces in filenames to make them web-friendly
      photo_filenames <- URLencode(readLines(file_list_url))
      
      shuffled_photo_urls(sample(paste0(nas_base_url, photo_filenames)))
      
    }, error = function(e) {
      f7Dialog(
        title = "Error",
        text = "Could not load the photo list from the NAS. Please check the `nas_base_url` in the code and ensure your NAS is accessible.",
        type = "alert"
      )
    })
  })
  
  # --- Shuffle Button Logic ---
  # This runs every time the "Shuffle Photos" button is clicked
  observeEvent(input$shuffle_button, {
    # Re-shuffle the list of URLs
    shuffled_photo_urls(sample(shuffled_photo_urls()))
    # Reset the view to show the first 10 new photos
    photos_to_display_count(10)
    # Scroll the user back to the top of the page
    runjs("window.scrollTo(0, 0);")
  })
  
  # --- Infinite Scroll Logic ---
  # This JavaScript code detects when the "scroll_trigger" div is visible on screen
  # When it is, it tells Shiny by setting `input$scroll_trigger_visible` to true
  runjs("
    var observer = new IntersectionObserver(function(entries) {
      if(entries[0].isIntersecting === true) {
        Shiny.setInputValue('scroll_trigger_visible', true, {priority: 'event'});
      }
    }, { threshold: [0] });
    observer.observe(document.querySelector('#scroll_trigger'));
  ")
  
  # This observer waits for the JavaScript signal from above
  observeEvent(input$scroll_trigger_visible, {
    # Add 10 more photos to the display count
    new_count <- photos_to_display_count() + 10
    
    # Make sure we don't try to show more photos than we have
    if (new_count > length(shuffled_photo_urls())) {
      new_count <- length(shuffled_photo_urls())
    }
    
    photos_to_display_count(new_count)
  })
  
  # --- Render the Photo Feed UI ---
  # This is the core reactive element that builds the HTML for our feed
  output$photo_feed <- renderUI({
    
    # Get the current list of shuffled URLs
    urls <- shuffled_photo_urls()
    # Get how many we need to show
    count <- photos_to_display_count()
    
    # Ensure we have URLs to display before trying to render them
    req(length(urls) > 0, count > 0)
    
    # Take the first `count` URLs from the shuffled list
    urls_to_render <- urls[1:min(count, length(urls))]
    
    # Use lapply to create an f7Card for each photo URL
    # f7Card provides a nice, Instagram-like container for each image
    lapply(urls_to_render, function(url) {
      f7Card(
        # The `tags$img` is the actual HTML image element
        tags$img(src = url, style = "width: 100%; height: auto; display: block;")
      )
    })
  })
}

# 5. RUN THE APP ----
shinyApp(ui, server)
