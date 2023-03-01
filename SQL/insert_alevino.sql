/* ####---- Inserindo dados na Tabela Alevino do DB ----####
   # Versão Shiny
   # Data: 13/02/2023
   # Autor: Carlos A. Zarzar
   # NULLIF(argument_1,argument_2) retorna NULL se argument_1=argument_2
*/
INSERT INTO alevino(
  id_fabricante,prod_ale,apelido,sexo,created_at)
VALUES(
  (SELECT id_fabricante FROM fabricante WHERE nome_fabricante = '{input$ale_fab}'),
  '{input$prod_ale}','{input$apelido}','{input$sexo}',Now()
);
