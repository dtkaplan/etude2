
#' @export
QuizUI <- function() {
  ns <- I # NS(id) # Couldn't get server to work in tutorial document
  # so I rendered moot the namespace stuff for modules
  tagList(
    shinyjs::useShinyjs(),  withMathJax(),
    tags$span(htmlOutput(ns("prompt_lead")), style = "font-size: 12pt;"),
    tags$span(uiOutput(ns("prompt")), style="font-size: 15pt;"),
    tags$div(uiOutput(ns("choice_buttons")), style="font-size: 12pt;"),
    tags$div(
      actionButton(ns("next_question"), "Next drill question ..."),
    ),
    tags$hr(),
    tags$div(htmlOutput(ns("feedback")), style="font-size:12pt;"),
    tags$div(uiOutput(ns("right_answer")), style="font-size:15pt;"),
    tags$div(textOutput(ns("score"), inline=TRUE),
             actionButton(ns("start_over"), "Reset score to 0/0")
    )
  )
}

#' @export
format_choices <- function(choice_str) {
  choice_str  # nothing, for now.
}

#' make a pseudo-random sequence that avoids runs
#' @export
make_sequence <- function(nchoices, n = 20) {
  set <- sample(nchoices,  3*n,  replace=TRUE)
  runs <- c(FALSE, set[-1] == set[-length(set)])
  twos <- c(FALSE, FALSE, set[-(1:2)] == set[-(length(set) - c(0, 1)) ])

  no_runs <- set[!runs & !twos]
  while  (length(no_runs) < n) {
    no_runs <- c(no_runs, rev(no_runs))
  }

  no_runs[1:n]
}

#' @export
# QuizServer <- function(id, questions) {
#     moduleServer(
#       id,



QuizServer <-  function(input, output, session, questions = Sinusoids(),
                        label = "drill4", recorder) {
  observeEvent(input$start_over, {
    correct_answer() # blank it out
    current_feedback() # blank it out
    show_correct_answer("") # blank it out
    n_asked(1)
    n_answered(0)
    n_correct(0)
    new_question()

  })

  prompt <- reactiveVal("Getting started.")
  this_question <- reactiveVal(NULL)
  current_feedback <- reactiveVal("")
  correct_answer <-  reactiveVal("")
  show_correct_answer <- reactiveVal("")
  n_asked <- reactiveVal(0)
  n_answered <- reactiveVal(0)
  n_correct <- reactiveVal(0)
  lead <- reactiveVal("What do you want me to do?")
  current_choices <- reactiveVal(NULL)
  sequence <- reactive({
    sample(etude2::make_sequence(nrow(questions), 100))
  })

  observeEvent(input$next_question, {
    correct_answer("")
    new_question()
  }, ignoreNULL  = FALSE )

  new_question <- reactive({
    n_asked(n_asked() + 1)
    k <- sequence()[n_asked()]
    this_question(
      frame_question(questions, #Items(),
                     ndistractor = 5,
                     k = k))
    # Set the instructions for the prompt
    lead(this_question()$lead)

    prompt(HTML(this_question()$prompt))
    current_choices(this_question()$choices)
    correct_answer(HTML(this_question()$right))
    current_feedback("")
    updateActionButton(session, "next_question",
                       label = "Waiting for your answer")
    disable("next_question")
  })

  observeEvent(input$next_question, {
    show_correct_answer("")
  })

  # observeEvent(input$check_answer, {
  #     if (is.null(input$answer)) return()
  observeEvent(input$answer, {
    if (input$answer) {
      n_correct(n_correct() + 1)
      current_feedback("")
      show_correct_answer("")
      updateActionButton(session, "next_question",
                         label = "<span  style=\"color: green;\">Right!</span> Press to go to next question.")

    } else {
      current_feedback("<span style=\"color: red;\">Sorry, the correct answer is</span>")
      show_correct_answer(correct_answer())
      updateActionButton(session, "next_question",
                         label = "Next drill question")

    }

    enable("next_question")


    n_answered(n_answered() + 1)

  })

  observeEvent(n_answered(), {
        quiz_state <-
        tibble::tibble(id = label,
                       question = "drill",
                       answer = paste(n_correct(), "/", n_answered()),
                       time = as.character(Sys.time()),
                       correct = n_answered() >= 10 && n_correct() >= 0.8*n_answered(),
                       attempts = 1,
                       history = "")
    recorder[[label]] <- quiz_state
  })
  output$prompt_lead <- renderText(HTML(lead()))
  output$choice_buttons <- renderUI({
    if (is.null(current_choices())) return()

    Tmp <- withMathJax(radioButtons("answer",  " ", inline=TRUE,
                                    selected = NA,
                                    choices = current_choices()
    ))

    # radioButtons() strips out internal "<" and ">", replacing them
    # with &lt; and &gt; respectively.
    # Undo this whereever it happens
    Tmp <- gsub("&lt;", "<", Tmp) %>%
      gsub("&gt;", ">", .) %>%
      HTML(.)

    3 -4
    Tmp
  })

  output$prompt <- renderUI(withMathJax(prompt()))
  output$score <- renderText(paste0("Score ", n_correct(), "/", n_answered()))
  output$feedback <- renderText(current_feedback())
  output$right_answer <- renderUI(withMathJax(show_correct_answer()))
  output$show_hash <- renderText({
    if (n_answered() == 0 || input$quiz != "FOR SUBMITTING") return("")
    learnrhash::encode_obj(
      tibble(
        lesson = input$quiz,
        n_correct = n_correct(),
        n_answered = n_answered(),
        when = Sys.time()
      )
    )
  })
  }

#' Create a question with random distractors
#' @export
frame_question  <- function(items, ndistractor = 5, direction = "forward",
                            k = sample(length(items), 1)) {
  base <- items[k,]
  distractors <- items[-k, ] %>%
    filter(
      # should match  the group of  the selected  item
      group == base$group,
      # should come from the same quiz structure
      id == base$id,
      # the correct  answer should be unique
      answer  != base$answer) %>%
    group_by(answer) %>%
    # Delete  duplicate answers
    filter(row_number() == 1) %>%
    ungroup() %>%
    sample_n(pmin(ndistractor, nrow(.)))

  forward_direction <-
    switch(base$direction,
           "both" = runif(1) > 0.5, # set at random
           "forward" = TRUE,
           "backward" = FALSE)

  res <- if (forward_direction) {
    list(prompt =  base$question,
         lead = base$forward,
         right = base$answer,
         choices = sample(c(base$answer, distractors$answer))
    )
  } else {
    list(prompt =  base$answer,
         lead = base$backward,
         right = base$question,
         choices = sample(c(base$question, distractors$question))
    )
  }

  tmp <- res$choices # still character strings
  res$choices <- as.list(tmp == res$right) # Logical vector: which choice is right

  # Process character strings in tmp, e.g. to latex, as image, ...
  res$right <- format_choices(res$right)
  res$prompt <- format_choices(res$prompt)
  names(res$choices) <- format_choices(tmp)


  return(res)


}

hashbox <- function(id) {
  x = shiny::verbatimTextOutput(id, TRUE)
  x$attribs$style = "white-space: pre-wrap;"
  x
}

#' @export
Sinusoids <- function(direction = c("forward", "both", "backward")) {
  direction <- match.arg(direction)
  tibble::tribble(
    ~ answer,  ~ group, ~ question,
    "$$d_t\\, f(t) \\equiv a\\sin(at)$$",          "trig",   "$$f(t) \\equiv \\sin(at)$$",
    "$$d_t\\, f(t) \\equiv a\\cos(t)$$",           "trig",   "$$f(t) \\equiv \\sin(t)$$",
    "$$d_t\\, f(t) \\equiv -a\\sin(at)$$",         "trig",   "$$f(t) \\equiv \\cos(at)$$",
    "$$d_t\\, f(t) \\equiv -a\\sin(t)$$",           "trig",   "$$f(t) \\equiv a \\cos(t)$$",
    "$$d_t\\, f(t) \\equiv a\\cos(t)$$",           "trig",   "$$f(t) \\equiv a \\sin(t)$$",
    "$$d_t\\, f(t) \\equiv -a^2 \\cos(at)$$",      "trig",   "$$f(t) \\equiv -a \\sin(at)$$",
    "$$d_t\\, f(t) \\equiv -a\\sin(t)$$",          "trig",   "$$f(t) \\equiv -a \\cos(t)$$",
    "$$d_t\\, f(t) \\equiv -a\\cos(at)$$",         "trig",   "$$f(t) \\equiv -\\sin(at)$$",
    "$$d_t\\, f(t) \\equiv - \\sin(-t)$$",         "trig",   "$$f(t) \\equiv -\\cos(-t)$$",
    "$$d_t\\, f(t) \\equiv \\sin(-t)$$",           "trig",   "$$f(t) \\equiv \\cos(-t)$$",
    "$$d_t\\, f(t) \\equiv - \\cos(-t)$$",         "trig",   "$$f(t) \\equiv \\sin(-t)$$"
  ) %>%
    mutate(id = "trig",
           direction = direction,
           forward = "Select the derivative",
           backward = "Select the anti-derivative")
}





