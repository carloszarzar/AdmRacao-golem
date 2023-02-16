/* ####---- Inserindo dados do Distribuidor (vendedor) e seu endereço ----####
   # Versão Shiny
   # Data: 26/01/2023
   # Autor: Carlos A. Zarzar
*/
WITH ins1 AS (
	INSERT INTO endereco(logrador,bairro,cidade,estado,num_ende,cep,referencia)
	VALUES ('{input$logrador_dis}','{input$bairro_dis}','{input$cidade_dis}','{input$estado_dis}','{input$num_ende_dis}','{input$cep_dis}','{input$ref_dis}')
	RETURNING id_endereco
   )
, ins2 AS (
	INSERT INTO telefone(celular,whatsapp)
	VALUES ('{input$tel_dis}',{input$whats_dis})
	RETURNING id_telefone
   )
, ins3 AS (
  SELECT f.id_fabricante
  FROM fabricante AS f
    WHERE f.nome_fabricante = '{input$fab_distribuidor}'
)
INSERT INTO distribuidor(nome_distribuidor,tipo_produto_dis,created_at, id_telefone, id_endereco,id_fabricante)
SELECT '{input$nome_dis}','{input$tipo_produto_dis}',Now(),id_telefone,id_endereco,id_fabricante FROM ins2, ins1, ins3;
