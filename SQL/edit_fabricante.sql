/* ####---- Editar dados do Fabricante ----####
   # Data: 07/02/2023
   # Autor: Carlos A. Zarzar
   # Descrição: Editar dados de multiplas tabelas
   # https://dbfiddle.uk/dXJlxyKL
   # https://dba.stackexchange.com/questions/215653/how-to-run-update-query-on-multiple-table-via-joins-simultaneously-on-postgres
   #
*/
WITH fabricante_update AS (
  UPDATE fabricante
  SET nome_fabricante = '{input$nome_fab_edit}',
      tipo_produto_fab = '{input$tipo_produto_fab_edit}',
      modified_at = Now()
  WHERE nome_fabricante = '{select}'
  RETURNING id_fabricante, tipo_produto_fab, id_endereco, id_telefone
)
, add AS (UPDATE endereco
  SET logrador = '{input$logrador_fab_edit}',
      bairro = '{input$bairro_fab_edit}',
      cidade = '{input$cidade_fab_edit}',
      estado = '{input$estado_fab_edit}',
      num_ende = '{input$num_ende_fab_edit}',
      cep = '{input$cep_fab_edit}',
      referencia = '{input$ref_fab_edit}'
  WHERE (id_endereco) IN (select id_endereco from fabricante_update)
  )
UPDATE telefone
  SET celular = '{input$tel_fab_edit}',
      whatsapp = '{input$whats_fab_edit}'
  WHERE (id_telefone) IN (select id_telefone from fabricante_update);
