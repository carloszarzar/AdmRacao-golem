/* ####---- Inserindo dados na Tabela Ração do DB ----####
   # Versão Shiny
   # Data: 09/02/2023
   # Autor: Carlos A. Zarzar
*/
/* WITH ins1 AS (
	INSERT INTO endereco(logrador,bairro,cidade,estado,num_ende,cep,referencia)
	VALUES ('{input$logrador_fab}','{input$bairro_fab}','{input$cidade_fab}','{input$estado_fab}','{input$num_ende_fab}','{input$cep_fab}','{input$ref_fab}')
	RETURNING id_endereco
   )
, ins2 AS (
	INSERT INTO telefone(celular,whatsapp)
	VALUES ('{input$tel_fab}',{input$whats_fab})
	RETURNING id_telefone
   )
INSERT INTO fabricante(nome_fabricante,tipo_produto_fab, id_telefone, id_endereco)
SELECT '{input$nome_fab}','{input$tipo_produto_fab}',id_telefone,id_endereco FROM ins2, ins1;
*/
