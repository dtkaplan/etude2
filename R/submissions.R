#' @rdname etude2_submissions
#' @name etude2_submissions
#' @title Elements to handle submissions from learnr tutorials for etude2
#'
#' #' @description
#' The following are addon element for learnr tutorials that enable the encoding and
#' hashed learnr solutions and shiny apps reporting using the `apps_state` system.
#'
#' Note that when including these functions in a learnr Rmd document it is necessary that
#' the logic functions, `*_logic()`, be included in an R chunk where `context="server"` as
#' they interact with the underlying Shiny functionality. Conversely, any of the ui functions,
#' `*_ui()`, must *not* be included in an R chunk with a `context`. Both types of functions
#' have been written to provide useful feedback if they detect they are in the wrong R chunk
#' type.
#'
#' The overall organization of the upper level functions comes from the `{learnrhash}` package. But
#' the internals are distinct from the system implemented by `{learnrhash}`.
#'
NULL

#' @rdname etude2_submissions
#' @export
submissions_logic <- function() {
  p <- parent.frame()
  learnrhash:::check_server_context(p)

  local({
    if (!exists("apps_state")) apps_state <- reactiveValues()

    shiny_state <- shiny::eventReactive(input$get_state, {
      dplyr::bind_rows(reactiveValuesToList(apps_state))
    })

    current_state <- shiny::eventReactive(input$get_state, {
      All <- learnr:::get_objects(session)
      objs = learnr:::get_all_state_objects(session)
      objs = learnr:::submissions_from_state_objects(objs)
      # Discard sandboxes without checking
      keepers <- rep(TRUE, length(objs))
      for(k in seq_along(objs)) {
        if (objs[[k]]$type == "exercise_submission") {
          if (!objs[[k]]$data$checked) {
            keepers[k] <- FALSE
          }
        }
      }

      return(objs[keepers])
    })
    current_state_report <- reactive({
      state_objs <- current_state()
      app_results <- shiny_state()

      # save(state_objs, app_results, file="~/Downloads/state.rda")
      if (length(state_objs) == 0 && nrow(shiny_state()) == 0) {
        return(tibble(message = "No submissions yet", history=1))
      }
      condense <- function(item) {
        if (item$type == "exercise_submission") {
          tibble::tibble(id = item$id,
                         question = "codebox",
                         answer = item$data$code,
                         time = format(Sys.time(), tz="GMT", usetz=TRUE),
                         correct = if (item$data$checked) item$data$feedback$correct else FALSE,
                         attempts = 999,
                         history=NA
          )
        } else { # question submission
          tibble::tibble(id=item$id,
                         question  = substr(item$data$question, 0, 20),
                         answer=item$data$answer,
                         time = format(item$data$time, tz="GMT", usetz=TRUE),
                         correct = ifelse(item$data$is_essay,
                                          NA, item$data$correct),
                         attempts = length(item$data$history)/2,
                         history = paste0(unlist(item$data$history),collapse=":::")
          )
        }
      }

      For_report <- lapply(state_objs, condense) %>%
        dplyr::bind_rows()

      if (inherits(app_results, "data.frame"))
        For_report <- bind_rows(For_report, app_results)

      if (exists("document_roster")) {
        For_report <- For_report %>%
          dplyr::full_join(document_roster, by = "id")
      }

      For_report %>%
        dplyr::mutate(document = etude2::etude_document_name()) %>%
        dplyr::arrange(id)
    })
    output$current_answers <- renderTable({
      current_state_report() %>%
        dplyr::select(-history)
      })

    output$hash_output <- renderText({
      Res <- current_state_report()
      if (length(Res) == 2 && nrow(Res) <= 1) return("No responses have been recorded yet.")

      Res %>%
        dplyr::filter(!is.na(answer)) %>%
        learnrhash::encode_obj()
    })

  },
  envir = p)
}
#' @rdname etude2_submissions
#' @export
submissions_ui = function() {
  learnrhash:::check_not_server_context(parent.frame())
  shiny::tags$div(
    shiny::actionButton("get_state", "Refresh summary of responses"),
    learnrhash:::wrapped_verbatim_text_output("hash_output", TRUE),
    shiny::tableOutput("current_answers"),
    shiny::tags$br(),
    shiny::tags$br(),
    shiny::tags$br(), # just to provide some space
    # before the learnr advance/retreat buttons.
  )
}
