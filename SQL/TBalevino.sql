/* ####---- Tabela Alevino ----####
   # Data: 13/02/2023
   # Autor: Carlos A. Zarzar
   # Descrição: Tabela resumo dos Alevinos cadastrados
*/
/*
SELECT * FROM alevino AS ale
  INNER JOIN fabricante AS fab USING(id_fabricante)
  INNER JOIN distribuidor AS dis ON dis.id_fabricante = fab.id_fabricante
  INNER JOIN endereco AS ende ON ende.id_endereco = dis.id_endereco
  INNER JOIN telefone AS tel ON tel.id_telefone = dis.id_telefone
  ORDER BY ale.created_at DESC;
*/
SELECT fab.id_fabricante, ale.id_alevino, dis.id_distribuidor, fab.nome_fabricante,ale.prod_ale,ale.sexo,ale.apelido,
dis.nome_distribuidor,ende.logrador,ende.bairro,ende.cidade,ende.estado,ende.num_ende,ende.cep,ende.referencia,
tel.celular,tel.whatsapp, ale.created_at
FROM alevino AS ale
  LEFT JOIN fabricante AS fab USING(id_fabricante)
  LEFT JOIN distribuidor AS dis ON dis.id_fabricante = fab.id_fabricante
  LEFT JOIN endereco AS ende ON ende.id_endereco = dis.id_endereco
  LEFT JOIN telefone AS tel ON tel.id_telefone = dis.id_telefone
  ORDER BY ale.created_at DESC;
