/* ####---- Tabela Ração cadastradas ----####
   # Data: 06/02/2023
   # Autor: Carlos A. Zarzar
*/
SELECT DISTINCT nome AS "Nome da ração",
            tamanho AS "Tamanho pellet (mm)",
            tipo AS "Fase",
            id_fabricante "Fabricante",
            created_at AS "Data"
            FROM racao ORDER BY created_at;
