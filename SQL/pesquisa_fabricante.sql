/* ####---- Box Pesquisa Fabricante ----####
   # Data: 30/01/2023
   # Autor: Carlos A. Zarzar
   # Descrição: Filtra no banco de dados o Fabricante selecionado
   # e faz um INNER JOIN com tabela endereço e telefone
*/
SELECT f.nome_fabricante, e.logrador, e.bairro, e.cidade, e.estado, e.num_ende,
  e.cep, e.referencia, tel.celular, tel.whatsapp
  FROM fabricante AS f
INNER JOIN endereco AS e
    ON f.id_endereco = e.id_endereco
INNER JOIN telefone AS tel
    ON f.id_telefone = tel.id_telefone
WHERE f.nome_fabricante = '{select}';

