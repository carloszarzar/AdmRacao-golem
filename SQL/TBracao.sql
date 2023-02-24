/* ####---- Tabela Ração cadastradas ----####
   # Data: 06/02/2023
   # Autor: Carlos A. Zarzar
*/
SELECT DISTINCT id_racao,
            r.nome,
            r.tamanho,
            r.tipo AS "Fase",
            f.nome_fabricante "Fabricante",
            f.id_fabricante,
            r.proteina AS "Proteína",
            r.created_at AS "Data",
            d.nome_distribuidor AS "Distribuidor",
            d.id_distribuidor,
            t.celular AS "Celular",
            t.whatsapp AS "Whatsapp",
            r.extrato_etereo_min,
            r.umidade_max,
            r.mineral_max,
            r.fibra_max,
            r.calcio_min,
            r.calcio_max,
            r.fosforo_min,
            r.vitamina_c_min
FROM racao AS r
LEFT JOIN fabricante AS f USING(id_fabricante)
LEFT JOIN distribuidor AS d USING(id_fabricante)
LEFT JOIN telefone AS t ON t.id_telefone = d.id_telefone
ORDER BY r.created_at DESC;
