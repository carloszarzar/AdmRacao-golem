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
	id_endereco SERIAL NOT NULL REFERENCES endereco(id_endereco),
	id_telefone INT NULL REFERENCES telefone(id_telefone),
	tipo_produto_fab VARCHAR(15) NOT NULL,
	created_at TIMESTAMPTZ DEFAULT Now() 
);
-- TABELA distribuidor (vendedor)
CREATE TABLE distribuidor(
  id_distribuidor SERIAL PRIMARY KEY,
  nome_distribuidor VARCHAR (20) NOT NULL,
  id_telefone INT NOT NULL REFERENCES telefone(id_telefone),
  id_endereco SERIAL NOT NULL REFERENCES endereco(id_endereco),
  tipo_produto_dis VARCHAR(15) NOT NULL,
  created_at TIMESTAMPTZ DEFAULT Now(),
  id_fabricante SERIAL NOT NULL REFERENCES fabricante(id_fabricante) ON DELETE CASCADE
);
-- TABELA proprietario
CREATE TABLE proprietario(
	id_proprietario SERIAL PRIMARY KEY NOT NULL,
	nome VARCHAR(40) NOT NULL,
	id_telefone INT NOT NULL REFERENCES telefone(id_telefone),
	cpf INT NOT NULL
);
-- TABELA fazenda
CREATE TABLE fazenda(
	id_fazenda SERIAL PRIMARY KEY NOT NULL,
	nome VARCHAR(40) NOT NULL,
	id_endereco SERIAL NOT NULL REFERENCES endereco(id_endereco),
	cnpj INT NULL,
	id_telefone INT NOT NULL REFERENCES telefone(id_telefone),
	num_tanque INT NOT NULL,
	especie VARCHAR(20) NOT NULL,
	id_proprietario SERIAL NOT NULL REFERENCES proprietario(id_proprietario),
	sist_cultivo VARCHAR(20) NOT NULL,
	created_at TIMESTAMPTZ DEFAULT Now() 
);

-- TABELA racao
CREATE TABLE racao(
	id_racao SERIAL PRIMARY KEY NOT NULL,
	nome VARCHAR(20) NOT NULL,
	tamanho INT NOT NULL,
	tipo VARCHAR(20) NOT NULL,
	id_fabricante SERIAL NOT NULL REFERENCES fabricante(id_fabricante),
	proteina INT NOT NULL,
	created_at TIMESTAMPTZ DEFAULT Now() 
);

-- TABELA alevino
CREATE TABLE alevino(
	id_alevino SERIAL PRIMARY KEY NOT NULL,
	id_fabricante SERIAL NOT NULL REFERENCES fabricante(id_fabricante),
	especie VARCHAR(20) NOT NULL,
	sexo VARCHAR(10) NULL,
	created_at TIMESTAMPTZ DEFAULT Now() 
);

-- TABELA compra
CREATE TABLE compra(
	id_compra SERIAL PRIMARY KEY NOT NULL,
	quantidade_itens INT NOT NULL,
	valor_total NUMERIC NOT NULL,
	data_compra TIMESTAMP NOT NULL,
	id_fabricante SERIAL NOT NULL REFERENCES fabricante(id_fabricante),
	data_chegada TIMESTAMP NULL,
	tipo_compra VARCHAR(10) NOT NULL,
	created_at TIMESTAMPTZ DEFAULT Now() 
);

-- TABELA compra_racao
CREATE TABLE compra_racao(
	id_comp_racao SERIAL PRIMARY KEY NOT NULL,
	id_compra SERIAL NOT NULL REFERENCES compra(id_compra),
	id_racao SERIAL NOT NULL REFERENCES racao(id_racao),
	valor_uni NUMERIC NOT NULL,
	quantidade REAL NOT NULL,
	valor_entrada NUMERIC NOT NULL,
	validade DATE NOT NULL,
	cod_lote INT NOT NULL,
	created_at TIMESTAMPTZ DEFAULT Now() 
);

-- TABELA compra_alevino
CREATE TABLE compra_alevino(
	id_comp_alevino SERIAL PRIMARY KEY NOT NULL,
	id_compra SERIAL NOT NULL REFERENCES compra(id_compra),
	id_alevino SERIAL NOT NULL REFERENCES alevino(id_alevino),
	valor_uni NUMERIC NOT NULL,
	quantidade REAL NOT NULL,
	valor_entrada NUMERIC NOT NULL,
	peso REAL NOT NULL,
	dias INT NOT NULL,
	cod_lote INT NOT NULL,
	created_at TIMESTAMPTZ DEFAULT Now() 
);

-- TABELA saida_racao
CREATE TABLE saida_racao(
	id_saida_racao SERIAL PRIMARY KEY NOT NULL,
	quantidade REAL NOT NULL,
	valor_saida NUMERIC NOT NULL,
	id_fazenda SERIAL NOT NULL REFERENCES fazenda(id_fazenda),
	data_saida TIMESTAMP NOT NULL,
	id_comp_racao SERIAL NOT NULL REFERENCES compra_racao(id_comp_racao),
	id_racao SERIAL NOT NULL REFERENCES racao(id_racao),
	created_at TIMESTAMPTZ DEFAULT Now() 
);

-- TABELA saida_alevino
CREATE TABLE saida_alevino(
	id_saida_alevino SERIAL PRIMARY KEY NOT NULL,
	quantidade INT NOT NULL,
	valor_saida NUMERIC NOT NULL,
	id_fazenda SERIAL NOT NULL REFERENCES fazenda(id_fazenda),
	data_saida TIMESTAMP NOT NULL,
	id_comp_alevino SERIAL NOT NULL REFERENCES compra_alevino(id_comp_alevino),
	created_at TIMESTAMPTZ DEFAULT Now() 
);

