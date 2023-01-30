/* Criando Views e Materialized Views do Banco de Dados
* Data: 26/01/2023
* Autor: Carlos Antonio Zarzar */
------------------------------------------------------------------------------
-- Views para racao da fazenda
-- Criando Views
CREATE OR REPLACE VIEW rac_fazenda AS
	SELECT f.nome As "Nome da fazenda", sr.quantidade, r.tipo AS "Tipo de racao"
	FROM saida_racao AS sr
	INNER JOIN compra_racao AS cr
	ON cr.id_comp_racao = sr.id_comp_racao
	INNER JOIN racao AS r
	ON r.id_racao=cr.id_racao
	INNER JOIN fazenda AS f
	ON f.id_fazenda = sr.id_fazenda
	ORDER BY sr.id_fazenda, sr.quantidade DESC;

-- Selecionando a View
SELECT * FROM rac_fazenda;

-- Excluindo Views
-- DROP VIEW IF EXISTS rec_fazenda CASCADE;

---------------- Construcao Tabela OFICIAL --------------------------------------------
-- Tabela Para resumir o estoque
-- Materialized View
-- Vou criar a tabela estoque. Lista de produto racao, com a quantidade de entrada, saida e a soma (resultado)
-- Tabela Entrada Estoque
CREATE MATERIALIZED VIEW view_entrada AS
	SELECT r.id_racao, SUM(cr.quantidade) AS "entrada", SUM(cr.valor_entrada) AS "valor_entrada"
	FROM racao AS r
		INNER JOIN compra_racao AS cr
		ON r.id_racao = cr.id_racao
		GROUP BY r.id_racao
		ORDER BY r.id_racao
WITH DATA;

-- Materialized View
-- Tabela de Saida Estoque
CREATE MATERIALIZED VIEW view_saida AS
	SELECT r.id_racao, SUM(sr.quantidade) AS "saida", SUM(sr.valor_saida) AS "valor_saida"
	FROM racao AS r
		INNER JOIN compra_racao AS cr
		ON r.id_racao = cr.id_racao
		INNER JOIN saida_racao AS sr
		ON sr.id_comp_racao = cr.id_comp_racao
		GROUP BY r.id_racao
		ORDER BY r.id_racao
WITH DATA;

-- SELECT view
SELECT * FROM view_entrada;
SELECT * FROM view_saida;

-- Tabela do estoque (FINAL juntando as duas entrada e saida)
SELECT id_racao, ve.entrada - vs.saida AS quant_total, ve.valor_entrada - vs.valor_saida AS valor_total
	FROM view_entrada AS ve
	INNER JOIN view_saida AS vs
	USING (id_racao);
	
-- Excluir as Materialiezed View
-- DROP MATERIALIZED VIEW view_entrada;
-- DROP MATERIALIZED VIEW view_saida;
----------------------#----------------------#----------------------#



