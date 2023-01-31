/* ####---- Box Pesquisa Distribuidor ----####
   # Data: 31/01/2023
   # Autor: Carlos A. Zarzar
   # Descrição: Filtra no banco de dados do Distribuidor selecionado
   # e faz um INNER JOIN com tabela endereço e telefone
*/
SELECT d.nome_distribuidor,d.tipo_produto_dis,f.nome_fabricante,
  e.logrador, e.bairro, e.cidade, e.estado, e.num_ende,
  e.cep, e.referencia, tel.celular, tel.whatsapp
  FROM distribuidor AS d
  INNER JOIN endereco AS e
    ON d.id_endereco = e.id_endereco
  INNER JOIN telefone AS tel
    ON d.id_telefone = tel.id_telefone
  INNER JOIN fabricante AS f
    ON d.id_fabricante = f.id_fabricante
WHERE d.nome_distribuidor = '{select}';

