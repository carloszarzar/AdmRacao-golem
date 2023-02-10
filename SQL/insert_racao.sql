/* ####---- Inserindo dados na Tabela Ração do DB ----####
   # Versão Shiny
   # Data: 09/02/2023
   # Autor: Carlos A. Zarzar
   # NULLIF(argument_1,argument_2) retorna NULL se argument_1=argument_2
*/
INSERT INTO racao(nome,tamanho,tipo,id_fabricante,
            proteina,created_at,
            extrato_etereo_min,umidade_max,
            mineral_max,fibra_max,calcio_min,
            calcio_max,fosforo_min,vitamina_c_min)
VALUES
('{input$nome_rac}',{input$tamanho},'{input$tipo_rac}',
(SELECT id_fabricante FROM fabricante WHERE nome_fabricante = '{input$select_fab_rac}'),
{input$proteina},Now(),
NULLIF({input$extrato},0),NULLIF({input$umidade},0),
NULLIF({input$mineral},0),NULLIF({input$fibra},0),NULLIF({input$calcio[1]},0),
NULLIF({input$calcio[2]},60),NULLIF({input$fosforo},0),NULLIF({input$vitamina},0));
