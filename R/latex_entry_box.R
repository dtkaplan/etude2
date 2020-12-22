#' Widgets for latex typesetting in projects
#'
#' @export
latex_entry_box <- function(id){
HTML(glue::glue('
<table><tr>
<td id={NS(id, "feedback")} class="shiny-html-output" style="color: green; font-size:20px;"></td>
<td>   </td>
<td><input id={NS(id, "latex")} type="text" class="form-control" placeholder="Type Latex here." style="font-size: 15px"></input></td>
<td>   </td>
<td><span id={NS(id, "rendered")} class="shiny-html-output" style="font-size:20px;"></span></td>
</tr></table>'))
}
