/* ####---- Editar dados do Alevino ----####
   # Data: 14/02/2023
   # Autor: Carlos A. Zarzar
   # Descrição: Editar dados de multiplas tabelaso Alevino
*/
UPDATE alevino
  SET id_fabricante = (SELECT id_fabricante FROM fabricante
  	WHERE nome_fabricante = '{input$ale_fab_edit}'),
  	  prod_ale = '{input$prod_ale_edit}',
      especie = '{input$especie_edit}',
      sexo = '{input$sexo_edit}',
      peso_init = {input$peso_init_edit},
      data_init = '{input$data_init_edit}',
      dias_init = {input$dias_init_edit}
  WHERE id_alevino = {df_ale$id_alevino};
