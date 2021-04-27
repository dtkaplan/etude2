#' @export
newQ <- function() {
  this_doc <- rstudioapi::getActiveDocumentContext()
  contents <- this_doc$contents

  chunk_text <- r"(```{r CHUNKID, echo=FALSE, results="markup"
     etude2::etudeQ(
       r"[prompt]",
         r"[]",
         r"[]",
         r"[]",
         r"[]"
     )
```
)"
  chunk_text <- gsub("CHUNKID", as.hexmode(floor(stats::runif(1, 1, 16^7))), chunk_text)

  rstudioapi::insertText(
    chunk_text,
    id = this_doc$id)
}
