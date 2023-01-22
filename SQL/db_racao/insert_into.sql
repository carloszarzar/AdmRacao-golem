/* Inserindo dados no DB em cada uma das tabelas
* Data: 22/12/2022
* Autor: Carlos Antonio Zarzar */

-- Inserindo dados nas tabelas

-- TABELA enderoco
INSERT INTO endereco(logrador,bairro,cidade,numero,cep,referencia)
VALUES
('Av. João de Barros'	 ,'Boa Vista'	,'Recife'		 ,399 ,50030230,'Casa'),
('Rua Siqueira Campos'	 ,'Santo Amaro' ,'Recife'		 ,160 ,50030230,'Predio'),
('Av. Conde da Boa Vista','Boa Vista'	,'Recife'		 ,3311,5040023 ,'Edficio Prirapama'),
('Avenida Rio Branco'	 ,'Centro'	 	,'Rio de Janeiro',246 ,20040002,'Casa'),
('Rua Pereira Estéfano','Vila da Saúde','São Paulo',465,04144070,'Predio'),
('Av. João de Barros'	 ,'Boa Vista'	,'Recife'		 ,399 ,50030230,'Casa'),
('Rua Siqueira Campos'	 ,'Santo Amaro' ,'Recife'		 ,160 ,50030230,'Predio'),
('Av. Conde da Boa Vista','Boa Vista'	,'Recife'		 ,3311,5040023 ,'Edficio Prirapama'),
('Avenida Rio Branco'	 ,'Centro'	 	,'Rio de Janeiro',246 ,20040002,'Casa'),
('Rua Pereira Estéfano','Vila da Saúde','São Paulo',465,04144070,'Predio'),
('Av. João de Barros'	 ,'Boa Vista'	,'Recife'		 ,399 ,50030230,'Casa'),
('Rua Siqueira Campos'	 ,'Santo Amaro' ,'Recife'		 ,160 ,50030230,'Predio'),
('Av. Conde da Boa Vista','Boa Vista'	,'Recife'		 ,3311,5040023 ,'Edficio Prirapama'),
('Avenida Rio Branco'	 ,'Centro'	 	,'Rio de Janeiro',246 ,20040002,'Casa'),
('Rua Pereira Estéfano','Vila da Saúde','São Paulo',465,04144070,'Predio');

-- TABELA telefone
INSERT INTO telefone(numero1,whatsapp,numero2)
VALUES
(6545465,	6545465,	6545465),
(6546466,	6546466,	6546466),
(6546466,	6546466,	6546466),
(6644654,	6644654,	6644654),
(6465464,	6465464,	6465464),
(6546466,	6546466,	6546466),
(6464654,	6464654,	6464654),
(6546466,	6546466,	6546466),
(4545684,	4545684,	4545684),
(6548949,	6548949,	6548949),
(6487849,	6487849,	6487849),
(2654987,	9879879,	2654987);
INSERT INTO telefone(numero1,whatsapp)
VALUES
(4545684,	4545684),
(6548949,	6548949),
(6487849,	6487849);

-- TABELA fornecedor
INSERT INTO fornecedor(nome_vendedor,nome_fabricante,id_endereco,id_telefone,tipo_produto)
VALUES
('Carlos Valouro','Polinutri',1,1,'Racao'),
('Fabricio Aragao','Fosfish',2,2,'Racao'),
('Mayara Noruega','JM Alevino',3,3,'Alevino'),
('Rafael Miranda','AleProducoes',4,4,'Alevino');

-- TABELA proprietario
INSERT INTO proprietario(nome,id_telefone,cpf)
VALUES
('Carlos A.',5,321564649),
('Heitor M.',6,135446545),
('Luan G.',7,344562131),
('Rapha L.',8,354898454);

-- TABELA fazenda
INSERT INTO fazenda(nome,id_endereco,cnpj,id_telefone,num_tanque,especie,id_proprietario,sist_cultivo)
VALUES
('Fazenda nova',9,1234654,9,5,'tambaqui',1,'extensivo'),
('Fazenda Esperança',10,3549845,10,5,'tambaqui',2,'Semi-intensivo'),
('Fazenda Peixão',11,24757777,11,8,'tambaqui',3,'intensivo'),
('Fazenda Peixe bom',12,5465468,12,10,'tambaqui',4,'extensivo'),
('Fazenda Peixe bonito',13,4568799,13,15,'tambaqui',4,'extensivo');

-- TABELA racao
INSERT INTO racao(tamanho,tipo,id_fornecedor,proteina)
VALUES
(5,'alevino',1,48),
(10,'engorda',1,38),
(25,'prime',1,42),
(5,'alevino',4,48);

-- TABELA alevino
INSERT INTO alevino(id_fornecedor,especie,sexo)
VALUES
(2,'tambaqui','misto'),
(2,'tambaqui','macho'),
(3,'tambaqui','fêmea');

-- TABELA compra
INSERT INTO compra(quantidade_itens,valor_total,data_compra,id_fornecedor,data_chegada,tipo_compra)
VALUES
(2,8400,'2022-02-21 00:00:00',1,'2022-02-28 00:00:00','ração'),
(1,4000,'2022-05-05 00:00:00',1,'2022-06-01 00:00:00','ração'),
(1,1250,'2022-07-07 00:00:00',2,'2022-07-17 00:00:00','alevino'),
(3,29050,'2022-07-27 00:00:00',1,'2022-08-01 00:00:00','ração'),
(2,2650,'2022-09-30 00:00:00',2,'2022-10-30 00:00:00','alevino'),
(2,1995,'2022-10-05 00:00:00',2,'2022-10-15 00:00:00','alevino'),
(1,6250,'2022-11-10 00:00:00',4,'2022-12-20 00:00:00','ração');

-- TABELA compra_racao
INSERT INTO compra_racao(id_compra,id_racao,valor_uni,quantidade,valor_entrada,validade,cod_lote)
VALUES
(1,1,2.5,2000,5000,'2025-01-01',123),
(1,2,3.4,1000,3400,'2025-01-01',321),
(2,3,4.0,1000,4000,'2025-01-01',654),
(4,1,2.5,4000,10000,'2025-01-01',456),
(4,2,3.4,2000,6800,'2025-01-01',987),
(4,3,4.0,1500,6000,'2025-01-01',789),
(7,4,2.5,2500,6250,'2025-01-01',789);

-- TABELA compra_alevino
INSERT INTO compra_alevino(id_compra,id_alevino,valor_uni,quantidade,valor_entrada,peso,dias,cod_lote)
VALUES
(3,1,125,10,1250,5,10,123),
(5,2,125,10,1250,7,15,321),
(5,3,140,10,1400,5,12,654),
(6,2,125,7,875,6,12,456),
(6,3,140,8,1120,5,10,987);

-- TABELA saida_racao
INSERT INTO saida_racao(quantidade,valor_saida,id_fazenda,data_saida,id_comp_racao,id_racao)
VALUES
(2000,5000,1,'2022-03-05 00:00:00',1,1),
(1000,3400,2,'2022-05-08 00:00:00',2,2),
(500,1700,3,'2022-09-25 00:00:00',3,3),
(100,340,4,'2022-09-25 00:00:00',3,3),
(1000,2500,1,'2023-02-10 00:00:00',4,1),
(1000,2500,2,'2023-03-30 00:00:00',5,2),
(1000,2500,3,'2023-04-05 00:00:00',6,3),
(575,1437.5,4,'2023-11-10 00:00:00',4,1),
(1525,3812.5,4,'2023-12-15 00:00:00',4,1),
(1000,2500,1,'2023-12-20 00:00:00',7,4),
(1200,3000,2,'2023-12-20 00:00:00',7,4);

-- TABELA saida_alevino
INSERT INTO saida_alevino(quantidade,valor_saida,id_fazenda,data_saida,id_comp_alevino)
VALUES
(10,1250,1,'2022-07-07 00:00:00',1),
(20,2650,2,'2022-09-30 00:00:00',2),
(20,1995,2,'2022-09-30 00:00:00',3),
(7,875,3,'2022-10-05 00:00:00',4),
(8,1120,1,'2022-10-05 00:00:00',5);
