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
  SET nome_fabricante = 'JJ Alevino',
      tipo_produto_fab = 'Alevino',
      modified_at = Now()
  WHERE id_fabricante = 3
  RETURNING id_fabricante, tipo_produto_fab, id_endereco, id_telefone
)
, add AS (UPDATE endereco 
  SET logrador = 'Rua Agostinho',
      bairro = 'Candeias',
      cidade = 'Jaboatao',
      estado = 'PE',
      num_ende = '333',
      cep = '3333333',
      referencia = 'Pousada'
  WHERE (id_endereco) IN (select id_endereco from fabricante_update)
  )
UPDATE telefone
  SET celular = '333333333',
      whatsapp = 'f'
  WHERE (id_telefone) IN (select id_telefone from fabricante_update);
