#' cadastroDasb
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
cadastroDash <- function(){
  box(title = "Cadastro", status = "primary",
      width = 12, height = 260,
      box(title = "Forncedor", width = 3, background = "light-blue",
          "Cadastre um novo fornecedor", br(),
          actionButton('btnFornecedor', 'Cadastrar')
      ),
      box(title = "Ração", width = 3, background = "light-blue",
          "Adicionar tipos de rações fornecidas e suas características",
          br(),
          actionButton(inputId = 'btnRacao', 'Cadastrar')
      ),
      box(title = "Alevino", width = 3, background = "light-blue",
          "Adicionar tipos de Alevinos fornecidos",
          br(),
          actionButton('btnAlevino', 'Cadastrar')
      ),
      box(title = "Fazendas", width = 3, background = "light-blue",
          "Cadastrar fazendas parceiras",
          br(),
          actionButton('btnFazenda', 'Cadastrar')
      )
  )

}
