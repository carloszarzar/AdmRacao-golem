/* ####---- Inserindo dados na Tabela Saída Ração ----####
   # Data: 07/03/2023
   # Autor: Carlos A. Zarzar
   # Inserindo (mais de uma linha) dados na tabela saida_racao no DB
*/
INSERT INTO saida_racao(quantidade,valor_saida,id_fazenda,data_saida,id_comp_racao,id_racao)
  SELECT quantidade,valor_saida,id_fazenda,data_saida,id_comp_racao,id_racao FROM valores_a_inserir;
