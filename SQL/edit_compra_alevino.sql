/* ####---- Inserindo dados na Tabela Compra Alevino Edição ----####
   # Data: 01/03/2023
   # Autor: Carlos A. Zarzar
   # Inserindo dados editados na tabela compra_alevino no DB
*/
UPDATE compra_alevino
  SET data_init = '{data_init}', cod_lote = '{cod_lote}',
      dias_init = {dias_init}, peso_init = {peso_init}
  WHERE id_comp_alevino = {id_compAle_select};
