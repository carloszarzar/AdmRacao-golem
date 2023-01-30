/* ####---- Resumo box Fornecedor ----####
   # Data: 26/01/2023
   # Autor: Carlos A. Zarzar
*/
SELECT DISTINCT nome_fabricante AS "Fabricante",
            tipo_produto_fab AS "Produto",
            created_at AS "Data"
            FROM fabricante ORDER BY created_at DESC;
/* SELECT * FROM fabricante; */
