/* ####---- Tabela Proprietário cadastradas ----####
   # Data: 15/02/2023
   # Autor: Carlos A. Zarzar
*/
SELECT * FROM proprietario AS prop
INNER JOIN telefone USING(id_telefone)
ORDER BY prop.created_at DESC;
