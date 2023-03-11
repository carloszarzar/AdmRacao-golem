/* ####---- Editando dados na Tabela Saída ----####
   # Data: 09/03/2023
   # Autor: Carlos A. Zarzar
   # Editando dados na tabela saída no DB
*/
UPDATE saida
SET data_saida = '{input$data_saida_edit}', id_fazenda = (SELECT id_fazenda FROM fazenda WHERE nome = '{input$fazenda_edit}')
WHERE id_saida = {list_saida$id_saida};
