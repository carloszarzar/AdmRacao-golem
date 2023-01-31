/* ####---- Inserindo dados do Distribuidor (vendedor) e seu endereço ----####
   # Data: 26/01/2023
   # Autor: Carlos A. Zarzar
*/
-- Inserindo dados na tabela distribuidor + telefone + endereco, de uma vez e conectando os id's.
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
, ins3 AS (
  SELECT f.id_fabricante
  FROM fabricante AS f
    WHERE f.nome_fabricante = 'AleProducoes'
)
INSERT INTO distribuidor(nome_distribuidor,tipo_produto_dis,id_telefone,id_endereco,id_fabricante)
SELECT  'Joel Fraga','Ração',id_telefone,id_endereco,id_fabricante FROM ins2, ins1, ins3;
