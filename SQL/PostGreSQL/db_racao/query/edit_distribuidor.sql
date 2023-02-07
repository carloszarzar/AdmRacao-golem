/* ####---- Editar dados do Distribuidor ----####
   # Data: 07/02/2023
   # Autor: Carlos A. Zarzar
   # Descrição: Editar dados de multiplas tabelas
   # https://dbfiddle.uk/dXJlxyKL
   # https://dba.stackexchange.com/questions/215653/how-to-run-update-query-on-multiple-table-via-joins-simultaneously-on-postgres
   #
*/
WITH distribuidor_update AS (
  UPDATE distribuidor
  SET nome_distribuidor = 'Carlos Antonio',
      tipo_produto_dis = 'Alevino',
      id_fabricante = (
  SELECT f.id_fabricante
  FROM fabricante AS f
    WHERE f.nome_fabricante = 'AleProducoes'
),
      modified_at = Now()
  WHERE nome_distribuidor = 'Carlos Valouro'
  RETURNING id_endereco, id_telefone
)
, add AS (UPDATE endereco
  SET logrador = 'Rua sem nome',
      bairro = 'Qualquer',
      cidade = 'Qualquer',
      estado = 'Qualquer',
      num_ende = '8888',
      cep = '888888888',
      referencia = 'Qualquer'
  WHERE (id_endereco) IN (select id_endereco from distribuidor_update)
  )
UPDATE telefone
  SET celular = '888888888',
      whatsapp = 'f'
  WHERE (id_telefone) IN (select id_telefone from distribuidor_update);
