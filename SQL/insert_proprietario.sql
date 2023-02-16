/* ####---- Inserindo dados na Tabela Proprietário do DB ----####
   # Versão Shiny
   # Data: 15/02/2023
   # Autor: Carlos A. Zarzar
   # NULLIF(argument_1,argument_2) retorna NULL se argument_1=argument_2
*/
WITH ins1 AS (
	INSERT INTO telefone(celular,whatsapp)
	VALUES ('{input$tel_prop}',{input$whats_prop})
	RETURNING id_telefone
   )
INSERT INTO proprietario(nome,cpf,created_at,id_telefone)
SELECT '{input$nome_prop}','{input$cpf_prop}',Now(),id_telefone FROM ins1;
