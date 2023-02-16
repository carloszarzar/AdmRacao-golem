/* ####---- Inserindo dados na Tabela Fazenda do DB ----####
   # Versão Shiny
   # Data: 16/02/2023
   # Autor: Carlos A. Zarzar
*/
WITH ins1 AS (
	INSERT INTO endereco(logrador,bairro,cidade,estado,num_ende,cep,referencia)
	VALUES ('{input$logrador_faz}','{input$bairro_faz}','{input$cidade_faz}','{input$estado_faz}','{input$num_ende_faz}','{input$cep_faz}','{input$ref_faz}')
	RETURNING id_endereco
   )
, ins2 AS (
	INSERT INTO telefone(celular,whatsapp)
	VALUES ('{input$tel_faz}',{input$whats_faz})
	RETURNING id_telefone
   )
, ins3 AS (
  SELECT p.id_proprietario
  FROM proprietario AS p
    WHERE p.nome = '{input$prop_faz_selected}'
)
INSERT INTO fazenda(nome,cnpj,num_tanque,especie,sist_cultivo,created_at,id_endereco,id_telefone,id_proprietario)
SELECT '{input$nome_faz}','{input$cnpj_faz}','{input$num_tanque_faz}','{input$prod_faz}','{input$sist_cult_faz}',
        Now(),id_endereco,id_telefone,id_proprietario FROM ins1, ins2, ins3;
