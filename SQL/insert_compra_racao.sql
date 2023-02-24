/* ####---- Inserindo dados na Tabela Compra e Compra Ração ----####
   # Data: 24/02/2023
   # Autor: Carlos A. Zarzar
   # Inserindo dados na tabela compra e compra racao no DB
*/
WITH ins_compra AS (
	INSERT INTO compra(quantidade_itens,quantidade_total,valor_total,data_compra,data_chegada,tipo_compra)
	VALUES (2,520,1450,Now(),Now(),'ração')
	RETURNING id_compra
   )
INSERT INTO compra_racao(id_racao,id_fabricante,id_distribuidor,valor_uni,quantidade,valor_entrada,validade,cod_lote,id_compra)
  SELECT 1,1,1,2.5,220,550,Now(),'123qwe',id_compra FROM ins_compra
  UNION ALL
  SELECT 1,1,1,3,100,300,Now(),'234qwe',id_compra FROM ins_compra;
