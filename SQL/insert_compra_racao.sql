/* ####---- Inserindo dados na Tabela Compra e Compra Ração ----####
   # Data: 24/02/2023
   # Autor: Carlos A. Zarzar
   # Inserindo dados na tabela compra e compra racao no DB
*/
WITH ins_compra AS (
	INSERT INTO compra(quantidade_itens,quantidade_total,valor_total,data_compra,data_chegada,tipo_compra)
	VALUES ({insertCompra$quantidade_itens},{insertCompra$quantidade_total},{insertCompra$valor_total},'{insertCompra$data_compra}','{insertCompra$data_chegada}','ração')
	RETURNING id_compra
   )
INSERT INTO compra_racao(id_racao,id_fabricante,id_distribuidor,valor_uni,quantidade,valor_entrada,validade,cod_lote,cod_fab,id_compra)
  SELECT id_racao,id_fabricante,id_distribuidor,valor_uni,quantidade,valor_entrada,validade,(SELECT concat(cod_lote,id_compra)),cod_fab,id_compra FROM valores_a_inserir,ins_compra;
/*
WITH ins_compra AS (
	INSERT INTO compra(quantidade_itens,quantidade_total,valor_total,data_compra,data_chegada,tipo_compra)
	VALUES ({insertCompra$quantidade_itens},{insertCompra$quantidade_total},{insertCompra$valor_total},'{insertCompra$data_compra}','{insertCompra$data_chegada}','ração')
	RETURNING id_compra
   )
INSERT INTO compra_racao(id_racao,id_fabricante,id_distribuidor,valor_uni,quantidade,valor_entrada,validade,cod_fab,id_compra)
  SELECT id_racao,id_fabricante,id_distribuidor,valor_uni,quantidade,valor_entrada,validade,cod_fab,id_compra FROM valores_a_inserir,ins_compra;
*/
