/* ####---- Editar dados do Fabricante ----####
   # Data: 07/02/2023
   # Autor: Carlos A. Zarzar
   # Descrição: Editar dados de multiplas tabelas
   # https://dbfiddle.uk/dXJlxyKL
   # https://dba.stackexchange.com/questions/215653/how-to-run-update-query-on-multiple-table-via-joins-simultaneously-on-postgres
   #
*/
WITH distribuidor_update AS (
  UPDATE distribuidor
  SET nome_distribuidor = '{input$nome_dis_edit}',
      tipo_produto_dis = '{input$tipo_produto_dis_edit}',
      id_fabricante = (SELECT f.id_fabricante
                       FROM fabricante AS f
                         WHERE f.nome_fabricante = '{input$fab_dis_edit}'),
      modified_at = Now()
  WHERE nome_distribuidor = '{select}'
  RETURNING id_endereco, id_telefone, id_fabricante
)
, add AS (UPDATE endereco
  SET logrador = '{input$logrador_dis_edit}',
      bairro = '{input$bairro_dis_edit}',
      cidade = '{input$cidade_dis_edit}',
      estado = '{input$estado_dis_edit}',
      num_ende = '{input$num_ende_dis_edit}',
      cep = '{input$cep_dis_edit}',
      referencia = '{input$ref_dis_edit}'
  WHERE (id_endereco) IN (select id_endereco from distribuidor_update)
  )
UPDATE telefone
  SET celular = '{input$tel_dis_edit}',
      whatsapp = '{input$whats_dis_edit}'
  WHERE (id_telefone) IN (select id_telefone from distribuidor_update);
