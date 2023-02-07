/* ####---- Resumo box Distribuidores ----####
   # Data: 26/01/2023
   # Autor: Carlos A. Zarzar
*/
SELECT DISTINCT d.nome_distribuidor AS "Distribuidor",
            d.tipo_produto_dis AS "Produto",
            f.nome_fabricante As "Nome do Fabricante",
            d.created_at AS "Data"
            FROM distribuidor AS d
  INNER JOIN fabricante AS f
    ON f.id_fabricante = d.id_fabricante
  ORDER BY d.created_at DESC;
