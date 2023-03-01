/* ####---- Editar dados do Alevino ----####
   # Data: 14/02/2023
   # Autor: Carlos A. Zarzar
   # Descrição: Editar dados de multiplas tabelaso Alevino
*/
UPDATE alevino
  SET id_fabricante = (SELECT id_fabricante FROM fabricante
  	WHERE nome_fabricante = '{input$ale_fab_edit}'),
  	  prod_ale = '{input$prod_ale_edit}',
      apelido = '{input$apelido_edit}',
      sexo = '{input$sexo_edit}',
      modified_at = Now()
  WHERE id_alevino = {df_ale$id_alevino};
