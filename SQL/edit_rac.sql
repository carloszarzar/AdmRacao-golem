/* ####---- Editar dados da Ração ----####
   # Data: 10/02/2023
   # Autor: Carlos A. Zarzar
   # Descrição: Editar dados de multiplas tabelas
   # https://dbfiddle.uk/dXJlxyKL
   # https://dba.stackexchange.com/questions/215653/how-to-run-update-query-on-multiple-table-via-joins-simultaneously-on-postgres
  # NULLIF(argument_1,argument_2) retorna NULL se argument_1=argument_2
*/
UPDATE racao
  SET nome = '{input$nome_rac_edit}',
      tamanho = '{input$tamanho_edit}',
      tipo = '{input$tipo_rac_edit}',
      id_fabricante = (SELECT id_fabricante FROM fabricante
  WHERE nome_fabricante = '{input$rac_fab_edit}'),
      proteina = {input$proteina_edit},
      extrato_etereo_min = NULLIF({input$extrato_edit},0),
      umidade_max = NULLIF({input$umidade_edit},0),
      mineral_max = NULLIF({input$mineral_edit},0),
      fibra_max = NULLIF({input$fibra_edit},0),
      calcio_min = NULLIF({input$calcio_edit[1]},0),
      calcio_max = NULLIF({input$calcio_edit[2]},60),
      fosforo_min = NULLIF({input$fosforo_edit},0),
      vitamina_c_min = NULLIF({input$vitamina_edit},0),
      modified_at = Now()
  WHERE id_racao = {slect_id_racao};
