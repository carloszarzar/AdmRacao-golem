/* ####---- Inserindo dados na Tabela Compra Ração ----####
   # Data: 27/02/2023
   # Autor: Carlos A. Zarzar
   # Inserindo dados editados na tabela compra_racao no DB
*/
UPDATE compra_racao
  SET validade = '{val_edit}', cod_lote = '{cod_lote}'
  WHERE id_comp_racao = {id_compRac_select};
