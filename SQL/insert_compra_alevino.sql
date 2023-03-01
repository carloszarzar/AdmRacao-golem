/* ####---- Inserindo dados na Tabela Compra e Compra Alevino ----####
   # Data: 01/03/2023
   # Autor: Carlos A. Zarzar
   # Inserindo dados na tabela compra e compra Alevino no DB
*/
WITH ins_compra AS (
	INSERT INTO compra(quantidade_itens,quantidade_total,valor_total,data_compra,data_chegada,tipo_compra)
	VALUES ({insertCompra$quantidade_itens},{insertCompra$quantidade_total},{insertCompra$valor_total},'{insertCompra$data_compra}','{insertCompra$data_chegada}','alevino')
	RETURNING id_compra
   )
INSERT INTO compra_alevino(id_alevino,id_fabricante,id_distribuidor,valor_uni,quantidade,valor_entrada,peso_init,data_init,dias_init,cod_lote,id_compra)
  SELECT id_alevino,id_fabricante,id_distribuidor,valor_uni,quantidade,valor_entrada,peso_init,data_init,dias_init,cod_lote,id_compra FROM valores_a_inserir,ins_compra;
