/* ####---- Editar dados do Proprietário ----####
   # Data: 17/02/2023
   # Autor: Carlos A. Zarzar
   # Descrição: Editar dados de multiplas tabelas
*/
WITH update_prop AS (
  UPDATE proprietario
    SET nome = '{input$nome_prop_edit}',
        cpf = '{input$cpf_prop_edit}',
        modified_at = Now()
    WHERE id_proprietario = {df$id_proprietario}
    RETURNING id_telefone
)
UPDATE telefone
SET celular = '{input$tel_prop_edit}',
    whatsapp = '{input$whats_prop_edit}'
WHERE (id_telefone) IN (select id_telefone from update_prop);
