/* ####---- Resumo box Distribuidores ----####
   # Data: 26/01/2023
   # Autor: Carlos A. Zarzar
   # Script: Obtendo dados do distribuidor juntando com a tabela do fabricante
*/
SELECT * FROM distribuidor AS d
INNER JOIN fabricante USING(id_fabricante)
INNER JOIN endereco AS e ON d.id_endereco=e.id_endereco
INNER JOIN telefone AS tel ON d.id_telefone=tel.id_telefone
ORDER BY d.created_at DESC;
