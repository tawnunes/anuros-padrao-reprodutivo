![Status](https://img.shields.io/badge/Status-Em%20andamento-green) 

# Padrões Reprodutivos em Anuros: Uma Análise Latitude-dependente

**Autores:** Jonathan [📧](mailto:); Ruth Oliveira [📧](mailto:ruthbennoda@gmail.com); Tawane Yara Nunes [📧](mailto:taw.ynunes@gmail.com)

### Descrição

O presente projeto foi desenvolvido no âmbito da disciplina Ecologia Comportamental, ministrada pela Prof. Dra. Lilian Tonelli Manica nos Programas de Pós-Graduação em Ecologia e Conservação e em Zoologia da Universidade Federal do Paraná. Como projeto final, os alunos deveriam elaborar uma hipótese na área da ecologia comportamental, obter os dados e conduzir as análises necessárias para testá-la.

### Objetivos

Neste contexto, nós buscamos avaliar a relação entre o padrão reprodutivo e a extensão da área de ocorrência de múltiplas espécies de anuros e inferir sobre a influência latitudinal nestes padrões utilizando a família Ranidae como modelo de análise. 

### Hipótese e Predições

Nós testamos a hipótese de que se a reprodução prolongada depende de condições climáticas mais estáveis e habitats mais específicos, enquanto a reprodução explosiva depende de variações sazonais, então esperamos que: 

a) espécies de anuros com menor extensão da área de ocorrência apresentem reprodução predominantemente prolongada; 

b) espécies da família Ranidae que tê maior amplitude latitudinal de ocorrência apresentem reprodução predominantemente explosiva e;

c) espécies da família Ranidae com áreade ocorrência mais restrita próxima à região equatorial apresentem reprodução predominantemente prolongada.

### Bases de Dados

Para atingir o objetivo e testas a hipótese elaborada nós utilizamos os dados levantados por [Nali et al. (2014)](https://datadryad.org/stash/dataset/doi:10.5061/dryad.270sf), do [World Bank Global Species Database](https://datacatalog.worldbank.org/home) e da [Lista Vermelha da IUCN](https://www.iucnredlist.org/). Todas as manipulações de dados e análises estão documentadas neste repositório.

### Estrutura do Projeto

```
Projeto/
*    ├── data/
*    │   ├── raw
*    │   └── processed
     ├── docs/
*    ├── figures/
     ├── R/
*    └── README.md
```

Os dados brutos obtidos podem ser encontrados em ``` dados/raw```, para utilizá-los é necessário citar as fontes originais. Apenas os arquivos ```.shp``` não foram carregados, mas podem ser baixados por meiod e um login diretamente no site da IUCN.

Os arquivos ```.csv``` processados encontra-se em ```dados/processados```

No diretório ```docs```você encontrará em breve uma versão completa do relátório final contendo apresentação resultados e discussão.

No diretório ```figures```você encontra as figuras geradas para ilustrar os resultados e compor o relatório final.

No diretório ```R``` você encontra os scripts contendo as manipulações de dados e análises conduzidas.


#### Requerimentos:

```
Pacotes R:

- tidyverse
- gplot2
- lme4
- DHARMa
- sf
- car
- ggeffects
```
