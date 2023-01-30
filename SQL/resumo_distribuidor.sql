/* ####---- Resumo box Distribuidores ----####
   # Data: 26/01/2023
   # Autor: Carlos A. Zarzar
*/
SELECT DISTINCT nome_distribuidor AS "Distribuidor",
            tipo_produto_dis AS "Produto",
            created_at AS "Data"
            FROM distribuidor ORDER BY created_at DESC;
