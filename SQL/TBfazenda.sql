/* ####---- Tabela Fazenda cadastradas ----####
   # Data: 15/02/2023
   # Autor: Carlos A. Zarzar
*/
SELECT
   faz.id_fazenda,faz.nome,faz.cnpj,faz.num_tanque,
   faz.especie,faz.sist_cultivo,faz.created_at,faz.modified_at,
   en.logrador,en.bairro,en.cidade,en.estado,en.num_ende,
   en.cep,en.referencia,
   tel.celular,tel.whatsapp,
   prop.nome AS "nome_prop",prop.cpf
FROM fazenda AS faz
INNER JOIN endereco AS en USING(id_endereco)
INNER JOIN telefone AS tel USING(id_telefone)
INNER JOIN proprietario AS prop USING(id_proprietario)
ORDER BY faz.created_at DESC;
