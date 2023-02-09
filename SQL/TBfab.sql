/* ####---- Tabela Fabricante cadastradas ----####
   # Data: 09/02/2023
   # Autor: Carlos A. Zarzar
*/
SELECT * FROM fabricante
INNER JOIN endereco USING(id_endereco)
INNER JOIN telefone USING(id_telefone);
