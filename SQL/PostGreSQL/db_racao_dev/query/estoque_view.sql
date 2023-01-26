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
SELECT
    coalesce(e.id_racao,s.id_racao) as id, coalesce(e.qty,0) - coalesce(s.qty,0) AS quant_total,
    coalesce(e.valor,0) - coalesce(s.valor,0) AS valor_total
FROM
    entrada AS e
    LEFT JOIN salida AS s
    ON e.id_racao = s.id_racao
