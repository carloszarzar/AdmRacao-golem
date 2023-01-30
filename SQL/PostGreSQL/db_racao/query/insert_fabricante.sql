/* ####---- Inserindo dados do Fabricante e seu endereço ----####
   # Data: 26/01/2023
   # Autor: Carlos A. Zarzar
*/
-- Inserindo dados na tabela fornecedor + telefone + endereco, de uma vez e conectando os id's.
WITH ins1 AS (
	INSERT INTO endereco(logrador,bairro,cidade,estado,num_ende,cep,referencia)
	VALUES ('Av. João de Barros','Boa Vista','Recife','Pernambuco','399','50030230','Casa')
	-- ON CONFLICT DO NOTHING         -- optional addition in Postgres 9.5+
	RETURNING id_endereco
   )
, ins2 AS (
	INSERT INTO telefone(celular,whatsapp)
	VALUES ('6545465',TRUE)
	RETURNING id_telefone
   )
INSERT INTO fabricante(nome_fabricante,tipo_produto_fab, id_telefone, id_endereco)
SELECT 'Purim Rações','Ração',id_telefone,id_endereco FROM ins2, ins1;
