/* ####---- Inserindo dados na Tabela Saída Ração ----####
   # Data: 07/03/2023
   # Autor: Carlos A. Zarzar
   # Inserindo (mais de uma linha) dados na tabela saida_racao no DB
*/
WITH ins_saida AS (
	INSERT INTO saida(quantidade_itens,quantidade_total,valor_total,data_saida,tipo_compra,id_fazenda)
	VALUES ({insertSaida$quantidade_itens},{insertSaida$quantidade_total},{insertSaida$valor_total},'{insertSaida$data_saida}','{insertSaida$tipo_compra}',{insertSaida$id_fazenda})
	RETURNING id_saida
   )
INSERT INTO saida_racao(quantidade,valor_saida,data_saida,id_comp_racao,id_racao,cod_lote,cod_fab,id_saida)
  SELECT quantidade,valor_saida,data_saida,id_comp_racao,id_racao,cod_lote,cod_fab,id_saida FROM valores_a_inserir,ins_saida;
