/* Scrip minimal reproducible example (Script_mre)
* Data: 04/01/2023
* Autor: Carlos Antonio Zarzar */

-- Purpose: I would like to do a subtraction operation on two tables that
-- were joined (INNER JOIN) and grouped (GROUP BY) before.
-------------------#-------------------#-------------------#-------------------#
--## Creating database and tables ##--

-- Building Database
CREATE DATABASE db_racao;

-- TABLE racao
CREATE TABLE racao(
	id_racao SERIAL PRIMARY KEY NOT NULL,
	tamanho INT NOT NULL,
	tipo VARCHAR(20) NOT NULL,
	proteina INT NOT NULL
);

-- TABLE compra_racao
CREATE TABLE compra_racao(
	id_comp_racao SERIAL PRIMARY KEY NOT NULL,
	id_racao SERIAL NOT NULL REFERENCES racao(id_racao),
	valor_uni NUMERIC NOT NULL,
	quantidade REAL NOT NULL,
	valor_entrada NUMERIC NOT NULL,
	validade DATE NOT NULL,
	cod_lote INT NOT NULL
);

-- TABLE saida_racao
CREATE TABLE saida_racao(
	id_saida_racao SERIAL PRIMARY KEY NOT NULL,
	quantidade REAL NOT NULL,
	valor_saida NUMERIC NOT NULL,
	data_saida TIMESTAMP NOT NULL,
	id_comp_racao SERIAL NOT NULL REFERENCES compra_racao(id_comp_racao),
	id_racao SERIAL NOT NULL REFERENCES racao(id_racao)
);
-------------------#-------------------#-------------------#-------------------#
--## Inserting data into tables ##--
-- TABLE racao
INSERT INTO racao(tamanho,tipo,proteina)
VALUES
(5,'alevino',48),
(10,'engorda',38),
(25,'prime',42),
(5,'alevino',48);

-- TABLE compra_racao
INSERT INTO compra_racao(id_racao,valor_uni,quantidade,valor_entrada,validade,cod_lote)
VALUES
(1,2.5,2000,5000,'2025-01-01',123),
(2,3.4,1000,3400,'2025-01-01',321),
(3,4.0,1000,4000,'2025-01-01',654),
(1,2.5,4000,10000,'2025-01-01',456),
(2,3.4,2000,6800,'2025-01-01',987),
(3,4.0,1500,6000,'2025-01-01',789),
(4,2.5,2500,6250,'2025-01-01',789);

-- TABLE saida_racao
INSERT INTO saida_racao(quantidade,valor_saida,data_saida,id_comp_racao,id_racao)
VALUES
(2000,5000,'2022-03-05 00:00:00',1,1),
(1000,3400,'2022-05-08 00:00:00',2,2),
(500,1700,'2022-09-25 00:00:00',3,3),
(100,340,'2022-09-25 00:00:00',3,3),
(1000,2500,'2023-02-10 00:00:00',4,1),
(1000,2500,'2023-03-30 00:00:00',5,2),
(1000,2500,'2023-04-05 00:00:00',6,3),
(575,1437.5,'2023-11-10 00:00:00',4,1),
(1525,3812.5,'2023-12-15 00:00:00',4,1),
(1000,2500,'2023-12-20 00:00:00',7,4),
(1200,3000,'2023-12-20 00:00:00',7,4);
-------------------#-------------------#-------------------#-------------------#
--## Making the queries ##--

/* The problem:
* I would like to subtract two resulting tables, the "Entrada" table and the "Saida" table.
* Each tables are grouped by id.racao.*/

/* Not an "elegant" solution, i.e. a workaround:
* I transform each table resulting from the operation into another table (of the MATERIALIZED VIEW type).
* And then I do the third operation which is the query of the "Estoque" table*/

-- MATERIALIZED VIEW  Entrada
CREATE MATERIALIZED VIEW view_entrada AS
	SELECT r.id_racao, SUM(cr.quantidade) AS "entrada", SUM(cr.valor_entrada) AS "valor_entrada"
		FROM racao AS r
			INNER JOIN compra_racao AS cr
			ON r.id_racao = cr.id_racao
			GROUP BY r.id_racao
			ORDER BY r.id_racao
WITH DATA;

-- MATERIALIZED VIEW Saida
CREATE MATERIALIZED VIEW view_saida AS
	SELECT r.id_racao, SUM(sr.quantidade) AS "saida", SUM(sr.valor_saida) AS "valor_saida"
		FROM racao AS r
			INNER JOIN saida_racao AS sr
			ON r.id_racao = sr.id_racao
			GROUP BY r.id_racao
			ORDER BY r.id_racao
WITH DATA;

-- And finally the query with the "Estoque" Table (joining the two "Entrada" and "Saida" by group)
-- Estoque
SELECT id_racao, ve.entrada - vs.saida AS quant_total, ve.valor_entrada - vs.valor_saida AS valor_total
	FROM view_entrada AS ve
	INNER JOIN view_saida AS vs
	USING (id_racao);
	
-- This is the result I expect. 
-------------------#-------------------#-------------------#-------------------#
-- Now what I would like to do is do all the operations at once and then 
-- make the resulting table a MATERIALIZED VIEW for queries.

-- An idea of what you'd like (may help):
SELECT 
	(SELECT r.id_racao, SUM(cr.quantidade) FROM racao AS r
		INNER JOIN compra_racao AS cr
		ON r.id_racao = cr.id_racao
		GROUP BY r.id_racao)
	 -
	(SELECT r.id_racao, SUM(sr.quantidade)
	FROM racao AS r
		INNER JOIN saida_racao AS sr
		ON r.id_racao = sr.id_racao
		GROUP BY r.id_racao) 
-------------------#-------------------#-------------------#-------------------#
-- Solution of the problem.
with entrada as (
SELECT r.id_racao, SUM(cr.quantidade) AS qty, SUM(cr.valor_entrada) AS valor
        FROM racao AS r
            INNER JOIN compra_racao AS cr
            ON r.id_racao = cr.id_racao
            GROUP BY r.id_racao
            ORDER BY r.id_racao),
salida as (
  SELECT r.id_racao, SUM(sr.quantidade) AS qty, SUM(sr.valor_saida) AS valor
        FROM racao AS r
            INNER JOIN saida_racao AS sr
            ON r.id_racao = sr.id_racao
            GROUP BY r.id_racao
            ORDER BY r.id_racao)
select 
    coalesce(e.id_racao,s.id_racao) as id, coalesce(e.qty,0) - coalesce(s.qty,0) AS quant_total, 
    coalesce(e.valor,0) - coalesce(s.valor,0) AS valor_total
from
    entrada e left join salida s on e.id_racao = s.id_racao

