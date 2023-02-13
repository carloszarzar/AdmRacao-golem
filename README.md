
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ADMRação

<!-- badges: start -->
<!-- badges: end -->

A ração é um dos principais custo na produção aquícola. Esse insumo
representa de 65% a 80% das despesas totais nas fazendas de peixe e
camarão. A tabela 1 abaixo mostra a participação das rações no Custo
Operacional Efetivo (COE) em Manaus, São Paulo e Paraná (Revista CNA
Brasil, ano 3 - Edição 13 Junho de 2017).

<div
style="-webkit-column-count: 2; -moz-column-count: 2; column-count: 2; -webkit-column-rule: 1px dotted #e0e0e0; -moz-column-rule: 1px dotted #e0e0e0; column-rule: 1px dotted #e0e0e0;">

<div>

| Espécie          | Sistema de Produção | Participação Custo |
|------------------|---------------------|--------------------|
| Tambaqui         | viveiro escavado    | 78%                |
| Tambaqui Curumim | viveiro escavado    | 64%                |
| Tilápia          | tanque rede         | 72%                |
| Tilápia          | viveiro escavado    | 82,31%             |

</div>

<div>

<img src="https://github.com/carloszarzar/AdmRacao-golem/blob/master/man/figures/racao.jpg" alt="Ração peixe" style="height: 180px; width:180px;"/>

</div>

</div>

Considerando o contexto em regiões onde a aquicultura é incipiente e a
produção de peixes é característico da agricultura familiar, a compra e
administração desse principal insumo é mais desafiador. Portanto, afim
de fomentar essas regiões de grande potencial para a aquicultura e
baixar o principal custo da produção, a ração, desenvolvemos o software
ADMRação, uma aplicação Web que permite administrar estoques de compras
coletivas de ração para peixes e camarão, e assim incentivar a produção
aquícola de pequenos agricultores familiares na região de Monte Alegre
do estado do Pará.

## Instalação

ADMRação pode ser instalado através do próprio R (RStudio) com auxílio
do pacote **devtools** (caso não tenha instalado instalação abaixo) que
importa o App Shiny diretamente do repositório remoto Github. Para a
instalação do App ADMRação siga os passos abaixo:

``` r
# install.packages("devtools") # Se não tiver instalado o "devtools" package
devtools::install_github("carloszarzar/AdmRacao-golem")
```

## Exemplo

O projeto ainda está em desenvolvimento (qualquer colaborador será muito
bem vindo no projeto), porém o propósito do App é ser simples e bastante
funcional. Público alvo são grupos pequenos de produtores ou agricultur
familiar, associação ou uma cooperativa que tenham interesse em produzir
peixes. Abaixo segue alguns **prints** do aplicativo Web na sua
utilização.

### Aba Início

<a href="https://github.com/carloszarzar/AdmRacao-golem/blob/master/man/figures/app_inicio.png">
<img src="https://github.com/carloszarzar/AdmRacao-golem/blob/master/man/figures/app_inicio.png" width="630" height="252"/>
</a>

### Aba Cadastro de Fornecedores

<a href="https://github.com/carloszarzar/AdmRacao-golem/blob/master/man/figures/app_inicio.pnghttps://github.com/carloszarzar/AdmRacao-golem/blob/master/man/figures/app_fornecedor.png">
<img src="https://github.com/carloszarzar/AdmRacao-golem/blob/master/man/figures/app_fornecedor.png" width="630" height="252"/>
</a>

### Aba Cadastro de Rações

<a href="https://github.com/carloszarzar/AdmRacao-golem/blob/master/man/figures/app_racao.png">
<img src="https://github.com/carloszarzar/AdmRacao-golem/blob/master/man/figures/app_racao.png" width="630" height="252"/>
</a>

## Informações técnicas

Esse projeto foi escrito em linguagem de programação R com o framework
Shiny e o fluxo de trabalho Golem. Back-end para o gerenciamento do
banco de dados (objeto relacional) PostgreSQL e algumas personalização e
definição de estilo ao front-end HTML, CSS e alguns derivados do Shiny.
Portanto, colaboradores experientes ou iniciantes interessados no
projeto serão bem vindos. Podem entrar em contato com:

- E-mail: <carlos.zarzar@ufopa.edu.br>
- Professor: Dr. Carlos Antônio Zarzar;
- Universidade Federal do Oeste do Pará (Campus Monte Alegre);
