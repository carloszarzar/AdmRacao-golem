/* ####---- Editando dados na Tabela Compra ----####
   # Data: 27/02/2023
   # Autor: Carlos A. Zarzar
   # Editando dados editados na tabela compra no DB
*/
UPDATE compra
SET data_compra = '{input$data_pedido_edit}', data_chegada = '{input$data_chegada_edit}'
WHERE id_compra = {list_comp$id_compra};
