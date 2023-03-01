/* Criando tabelas
* Data: 26/01/2023
* Autor: Carlos Antonio Zarzar */

-- Criando Banco de dados 
CREATE DATABASE db_racao;
-- Dando permissao para o usuario zarzar
GRANT ALL ON DATABASE db_racao TO zarzar;

-- Apagando banco de dados (antigo db_adm_racao)
-- DROP DATABASE db_admo_racao

-- TABELA endereco
CREATE TABLE endereco(
	id_endereco SERIAL PRIMARY KEY,
	logrador VARCHAR(40) NULL,
	bairro VARCHAR(30) NULL,
	cidade VARCHAR(30) NULL,
	estado VARCHAR(30) NULL,
	num_ende VARCHAR(10) NULL,
	cep VARCHAR(15) NULL,
	referencia TEXT NULL
);
-- TABELA telefone
CREATE TABLE telefone(
	id_telefone SERIAL PRIMARY KEY,
	celular VARCHAR(15) NOT NULL,
	whatsapp BOOLEAN NOT NULL
);
-- TABELA fabricante (Fornecedor)
CREATE TABLE fabricante(
	id_fabricante SERIAL PRIMARY KEY,
	nome_fabricante VARCHAR(20) NOT NULL,
	id_endereco SERIAL NOT NULL REFERENCES endereco(id_endereco) ON DELETE CASCADE,
	id_telefone INT NULL REFERENCES telefone(id_telefone) ON DELETE CASCADE,
	tipo_produto_fab VARCHAR(15) NOT NULL,
	created_at TIMESTAMPTZ DEFAULT Now(),
	modified_at TIMESTAMPTZ NULL 
);

-- TABELA distribuidor (vendedor)
CREATE TABLE distribuidor(
  id_distribuidor SERIAL PRIMARY KEY,
  nome_distribuidor VARCHAR (20) NOT NULL,
  id_telefone INT NOT NULL REFERENCES telefone(id_telefone) ON DELETE CASCADE,
  id_endereco SERIAL NOT NULL REFERENCES endereco(id_endereco) ON DELETE CASCADE,
  tipo_produto_dis VARCHAR(15) NOT NULL,
  created_at TIMESTAMPTZ DEFAULT Now(),
  modified_at TIMESTAMPTZ NULL,
  id_fabricante SERIAL NOT NULL REFERENCES fabricante(id_fabricante) ON DELETE CASCADE
);
-- TABELA proprietario
CREATE TABLE proprietario(
	id_proprietario SERIAL PRIMARY KEY NOT NULL,
	nome VARCHAR(40) NOT NULL,
	id_telefone INT NOT NULL REFERENCES telefone(id_telefone) ON DELETE CASCADE,
	cpf VARCHAR(11) NOT NULL,
	created_at TIMESTAMPTZ DEFAULT Now(),
	modified_at TIMESTAMPTZ NULL
);
-- TABELA fazenda
CREATE TABLE fazenda(
	id_fazenda SERIAL PRIMARY KEY NOT NULL,
	nome VARCHAR(40) NOT NULL,
	id_endereco SERIAL NOT NULL REFERENCES endereco(id_endereco) ON DELETE CASCADE,
	cnpj VARCHAR(14) NULL,
	id_telefone INT NOT NULL REFERENCES telefone(id_telefone) ON DELETE CASCADE,
	num_tanque INT NOT NULL,
	especie VARCHAR(20) NOT NULL,
	id_proprietario SERIAL NOT NULL REFERENCES proprietario(id_proprietario) ON DELETE CASCADE,
	sist_cultivo VARCHAR(20) NOT NULL,
	created_at TIMESTAMPTZ DEFAULT Now(),
	modified_at TIMESTAMPTZ NULL
);

-- TABELA racao
CREATE TABLE racao(
	id_racao SERIAL PRIMARY KEY NOT NULL,
	nome VARCHAR(20) NOT NULL,
	tamanho REAL NOT NULL, -- mm
	tipo VARCHAR(20) NOT NULL, -- alevino, juvenil 1, juvenil 2, engorda, finalização
	id_fabricante SERIAL NOT NULL REFERENCES fabricante(id_fabricante) ON DELETE CASCADE,
	proteina REAL NOT NULL, -- %
	created_at TIMESTAMPTZ DEFAULT Now(), 
	extrato_etereo_min REAL NULL, -- g/kg
    	umidade_max REAL NULL, -- %
    	mineral_max REAL NULL, -- g/kg
    	fibra_max REAL NULL, -- g/kg
    	calcio_min REAL NULL, -- g/kg
    	calcio_max REAL NULL, -- g/kg
    	fosforo_min REAL NULL, -- g/kg
    	vitamina_c_min REAL NULL, -- mg/kg
    	modified_at TIMESTAMPTZ NULL 
);

-- TABELA alevino
CREATE TABLE alevino(
	id_alevino SERIAL PRIMARY KEY NOT NULL,
	id_fabricante SERIAL NOT NULL REFERENCES fabricante(id_fabricante) ON DELETE CASCADE,
	prod_ale VARCHAR(20) NOT NULL,
	apelido VARCHAR(20) NULL, -- apelido do alevino
	sexo VARCHAR(10) NULL,
	peso_init REAL NOT NULL, -- mg (APAGAR Sair daqui)
	data_init TIMESTAMPTZ NOT NULL, -- Data de eclosão (APAGAR Sair daqui)
	dias_init INT NULL, -- dias de vida (APAGAR Sair daqui)
	created_at TIMESTAMPTZ DEFAULT Now(),
	modified_at TIMESTAMPTZ NULL 
);

-- TABELA compra
CREATE TABLE compra(
	id_compra SERIAL PRIMARY KEY NOT NULL,
	quantidade_itens INT NOT NULL,
    	quantidade_total NUMERIC NOT NULL, -- quantidade total comprada kg de ração ou milheiro de alevino
	valor_total NUMERIC NOT NULL,
	data_compra TIMESTAMP NOT NULL, -- Data que foi pago e realizado o pedido
	data_chegada TIMESTAMP NULL, -- Previsão de chegada
	tipo_compra VARCHAR(10) NOT NULL,
	created_at TIMESTAMPTZ DEFAULT Now() 
);

-- TABELA compra_racao
CREATE TABLE compra_racao(
	id_comp_racao SERIAL PRIMARY KEY NOT NULL,
	id_compra SERIAL NOT NULL REFERENCES compra(id_compra) ON DELETE CASCADE,
	id_racao SERIAL NOT NULL REFERENCES racao(id_racao) ON DELETE CASCADE,
     	id_fabricante SERIAL NOT NULL REFERENCES fabricante(id_fabricante) ON DELETE CASCADE,
    	id_distribuidor SERIAL NOT NULL REFERENCES distribuidor(id_distribuidor) ON DELETE CASCADE,
	valor_uni NUMERIC NOT NULL, -- Real R$/kg da ração
	quantidade REAL NOT NULL, -- quantidade comprada (kg)
	valor_entrada NUMERIC NOT NULL, -- Valor total da compra para essa ração (R$)
	validade VARCHAR(10) NOT NULL, -- Corrigir validade para = TIMESTAMPTZ NOT NULL
	cod_lote VARCHAR(30) NULL, -- Código da ração do fabricante para rastreio
	created_at TIMESTAMPTZ DEFAULT Now() 
);

-- TABELA compra_alevino
CREATE TABLE compra_alevino(
	id_comp_alevino SERIAL PRIMARY KEY NOT NULL,
	id_compra SERIAL NOT NULL REFERENCES compra(id_compra) ON DELETE CASCADE,
	id_alevino SERIAL NOT NULL REFERENCES alevino(id_alevino) ON DELETE CASCADE,
	id_fabricante SERIAL NOT NULL REFERENCES fabricante(id_fabricante) ON DELETE CASCADE,
	id_distribuidor SERIAL NOT NULL REFERENCES distribuidor(id_distribuidor) ON DELETE CASCADE,
	valor_uni NUMERIC NOT NULL, -- valor do milheiro
	quantidade REAL NOT NULL, -- quanto milheiros
	valor_entrada NUMERIC NOT NULL, -- quanto no total ta pagando (R$ x quant.)
	peso_init REAL NOT NULL, -- g
	data_init TIMESTAMPTZ NOT NULL, -- Data de eclosão (nascimento)
	dias_init INT NULL, -- dias de vida
	cod_lote VARCHAR(30) NULL,
	created_at TIMESTAMPTZ DEFAULT Now(),
	modified_at TIMESTAMPTZ NULL
);

-- TABELA saida_racao
CREATE TABLE saida_racao(
	id_saida_racao SERIAL PRIMARY KEY NOT NULL,
	quantidade REAL NOT NULL,
	valor_saida NUMERIC NOT NULL,
	id_fazenda SERIAL NOT NULL REFERENCES fazenda(id_fazenda) ON DELETE CASCADE,
	data_saida TIMESTAMP NOT NULL,
	id_comp_racao SERIAL NOT NULL REFERENCES compra_racao(id_comp_racao) ON DELETE CASCADE,
	id_racao SERIAL NOT NULL REFERENCES racao(id_racao) ON DELETE CASCADE,
	created_at TIMESTAMPTZ DEFAULT Now() 
);

-- TABELA saida_alevino
CREATE TABLE saida_alevino(
	id_saida_alevino SERIAL PRIMARY KEY NOT NULL,
	quantidade INT NOT NULL,
	valor_saida NUMERIC NOT NULL,
	id_fazenda SERIAL NOT NULL REFERENCES fazenda(id_fazenda) ON DELETE CASCADE,
	data_saida TIMESTAMP NOT NULL,
	id_comp_alevino SERIAL NOT NULL REFERENCES compra_alevino(id_comp_alevino) ON DELETE CASCADE,
	created_at TIMESTAMPTZ DEFAULT Now() 
);

