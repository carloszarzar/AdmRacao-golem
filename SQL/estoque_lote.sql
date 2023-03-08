/* ####---- Resumo do estoque na aba inicio (tab body) ----####
   # Data: 26/01/2023
   # Autor: Carlos A. Zarzar
   # OBS: Aqui estamos pegando cada lote para fazer q quantidade no estoque
*/
with entrada as (
SELECT cr.id_racao,cr.id_comp_racao, SUM(cr.quantidade) AS qty, SUM(cr.valor_entrada) AS valor
  FROM compra_racao AS cr
  GROUP BY cr.id_comp_racao
  ORDER BY cr.id_comp_racao),
salida as (
  SELECT sr.id_racao,sr.id_comp_racao, SUM(sr.quantidade) AS qty, SUM(sr.valor_saida) AS valor
  FROM saida_racao AS sr
  GROUP BY sr.id_racao,sr.id_comp_racao
  ORDER BY sr.id_comp_racao)
SELECT
    coalesce(e.id_racao,s.id_racao) as id_racao,
    coalesce(e.id_comp_racao,s.id_comp_racao) as id_comp_racao, coalesce(e.qty,0) - coalesce(s.qty,0) AS quant_total,
    coalesce(e.valor,0) - coalesce(s.valor,0) AS valor_total
FROM
    entrada AS e
    LEFT JOIN salida AS s
    ON e.id_comp_racao = s.id_comp_racao
