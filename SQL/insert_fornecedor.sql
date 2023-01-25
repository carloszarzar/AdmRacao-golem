/* ####---- Inserindo dados do FOrnecedor e seu endereço ----####
   # Data: 25/01/2023
   # Autor: Carlos A. Zarzar
*/

WITH ins1 AS (
	INSERT INTO endereco(logrador,bairro,cidade,numero,cep,referencia)
	VALUES ('{input$logrador}','{input$bairro}','{input$cidade}',{input$numero},{input$cep},'{input$referencia}')
	RETURNING id_endereco
   )
, ins2 AS (
	INSERT INTO telefone(numero1,whatsapp,numero2)
	VALUES ({input$telvendedor},	6545465,	{input$telfabricante})
	RETURNING id_telefone
   )
INSERT INTO fornecedor(nome_vendedor, nome_fabricante,tipo_produto, id_telefone, id_endereco)
SELECT '{input$nome_vendedor}','{input$nome_fabricante}','{input$tipo_produto}',id_telefone,id_endereco FROM ins2, ins1;
