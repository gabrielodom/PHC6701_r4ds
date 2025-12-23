# Check for Broken Hyperlinks
# Gabriel Odom and ChatGPT
# 2025-12-19



######  Overview of the Problem  ##############################################
library(R.utils)
library(tidyverse)
lessonLengths_int <-
  list.files(path = "lessons", full.names = TRUE) %>%
  map_int(.f = countLines)
sum(lessonLengths_int)
# I have nearly 7k lines of Quarto code. Scattered generously throughout these
# lines are many hyperlinks, which will decay over time. In fact, some are
# already broken even though the first edition of this text is only two years
# old. I need a programmatic solution to detect broken hyperlinks



######  Helper Functions  #####################################################

# install.packages("commonmark")
# install.packages("xml2")
# install.packages("httr2")

ExtractLinks <- function(file) {
  # browser()

  links <- commonmark::markdown_xml(readLines(file, warn = FALSE))

  tibble::tibble(
    file = file,
    url = links %>%
      # character to xml with namespace d1 <-> http://commonmark.org/xml/1.0
      xml2::read_xml() %>%
      # look for links inside of namespace d1.
      # NOTE: this is NOT a regex, but Xpath query language:
      # https://www.ionos.com/digitalguide/websites/web-development/xpath-tutorial/
      xml2::xml_find_all(".//d1:link") %>%
      # pull out the websites as character strings
      xml2::xml_attr("destination")
  )

}

# Test
ExtractLinks(file = "lessons/lesson14_github.qmd")
# # A tibble: 7 × 2
# file                        url
# <chr>                       <chr>
#   1 lessons/lesson14_github.qmd https://happygitwithr.com/big-picture.html
# 2 lessons/lesson14_github.qmd https://happygitwithr.com/github-acct.html
# 3 lessons/lesson14_github.qmd https://happygitwithr.com/shell.html#shell
# 4 lessons/lesson14_github.qmd https://happygitwithr.com/hello-git.html
# 5 lessons/lesson14_github.qmd https://happygitwithr.com/ssh-keys.html
# 6 lessons/lesson14_github.qmd https://happygitwithr.com/push-pull-github.html
# 7 lessons/lesson14_github.qmd https://happygitwithr.com/rstudio-git-github.html


IsBrokenLink <- function(url) {
  # browser()

  tryCatch({
    req <- httr2::request(url) %>%
      httr2::req_method("HEAD") %>%
      httr2::req_timeout(10)

    # If the website exists, then this call will be fine. If there is a
    # problem, then the req_perform() call will error. Thus the tryCatch()
    resp <- httr2::req_perform(req)
    httr2::resp_status(resp) >= 400
  }, error = function(e) {
    TRUE
  })

}

# Test
IsBrokenLink("https://happygitwithr.com/big-picture.html")
# [1] FALSE
# I checked this link on 2025-12-19 and it worked

IsBrokenLink("https://happygitwithr.com/some-link-that-doesnt-work.html")
# [1] TRUE


######  Find Broken Links  ####################################################
links_df <-
  list.files(
    path = "lessons",
    pattern = ".qmd$",
    full.names = TRUE
  ) %>%
  purrr::map_dfr(ExtractLinks) %>%
  mutate(is_broken = purrr::map_lgl(url, IsBrokenLink))

sum(links_df$is_broken)
# 15 as of 2025-12-19. That's not bad.

# UPDATE 2025-12-23: my code flags some DOI links as false positives. Maybe
# because of all the redirects?
# This is also a false positive: https://www.rapidtables.com/web/color/RGB_Color.html
# We are down to 5 false positive links:
# A tibble: 5 × 3
# file                                      url                           is_broken
# <chr>                                     <chr>                         <lgl>
# 1 lessons/lesson01_greater_data_science.qmd https://doi.org/10.1080/1061… TRUE
# 2 lessons/lesson01_greater_data_science.qmd https://doi.org/10.1080/1061… TRUE
# 3 lessons/lesson01_greater_data_science.qmd https://doi.org/10.1001/jama… TRUE
# 4 lessons/lesson04_ggplot.qmd               https://ieeexplore.ieee.org/… TRUE
# 5 lessons/lesson04s_examples.qmd            https://www.rapidtables.com/… TRUE
