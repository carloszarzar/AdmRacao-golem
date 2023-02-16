INSERT INTO racao(nome,tamanho,tipo,id_fabricante,
            proteina,created_at,
            extrato_etereo_min,umidade_max,
            mineral_max,fibra_max,calcio_min,
            calcio_max,fosforo_min,vitamina_c_min)
VALUES
('TESTE',2,'Alevino',
(SELECT id_fabricante FROM fabricante WHERE nome_fabricante = 'Purim Rações'),
32,Now(),
NULLIF(1,0),NULLIF(1,0),
NULLIF(1,0),NULLIF(1,0),NULLIF(1,0),
NULLIF(1,60),NULLIF(1,0),NULLIF(1,0));

