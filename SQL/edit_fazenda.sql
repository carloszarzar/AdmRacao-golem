/* ####---- Editar dados da Fazenda ----####
   # Data: 17/02/2023
   # Autor: Carlos A. Zarzar
   # Descrição: Editar dados de multiplas tabelas
*/
WITH up_faz AS (
  UPDATE fazenda
    SET nome = '{input$nome_faz_edit}',
        cnpj = '{input$cnpj_faz_edit}',
        num_tanque = {input$num_tanque_faz_edit},
        especie = '{input$prod_faz_edit}',
        id_proprietario = (
        SELECT p.id_proprietario
        FROM proprietario AS p
          WHERE p.nome = '{input$prop_faz_selected_edit}'
        ),
        sist_cultivo = '{input$sist_cult_faz_edit}',
        modified_at = Now()
    WHERE id_fazenda = {df$id_fazenda}
    RETURNING id_telefone,id_endereco
),
up_end AS (
  UPDATE endereco
  SET logrador = '{input$logrador_faz_edit}',
      bairro = '{input$bairro_faz_edit}',
      cidade = '{input$cidade_faz_edit}',
      estado = '{input$estado_faz_edit}',
      num_ende = '{input$num_ende_faz_edit}',
      cep = '{input$cep_faz_edit}',
      referencia = '{input$ref_faz_edit}'
  WHERE (id_endereco) IN (select id_endereco from up_faz)
)
UPDATE telefone
SET celular = '{input$tel_faz_edit}',
    whatsapp = '{input$whats_faz_edit}'
WHERE (id_telefone) IN (select id_telefone from up_faz);
