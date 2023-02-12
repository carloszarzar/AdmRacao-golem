/* ####---- Tabela Fabricante cadastradas ----####
   # Data: 09/02/2023
   # Autor: Carlos A. Zarzar
*/
SELECT * FROM fabricante AS f
INNER JOIN endereco USING(id_endereco)
INNER JOIN telefone USING(id_telefone)
ORDER BY f.created_at DESC;
