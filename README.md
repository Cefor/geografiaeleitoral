# Análise Comparada dos Modelos de Medição da Geografia Eleitoral: estudo da efetividade por meio de abordagem quantitativa

### Autor: Mauricio de Moares Rêgo Soares
### Orientador: Fabiano Peruzzo Schwartz

## Introdução

O presente repositório foi produzido a partir de pesquisa realizada no âmbito do curso Mestrado Profissional em Poder Legislativo e do grupo de pesquisa [Ciência de Dados Aplicada ao Estudo do Poder Legislativo: abordagem computacional e métodos de análise](http://dgp.cnpq.br/dgp/espelhogrupo/9712095383739020), com o objetivo avaliar, de forma comparativa, o potencial explicativo dos principais índices discutidos na literatura para a medição da geografia eleitoral brasileira, conhecidos por N, G e I de Moran, juntamente com um novo indicador, proposto na pesquisa, o índice M.

O novo indicador se apresenta como alternativa de medição que leva em consideração o peso proporcional das áreas (ou municípios) testadas em relação aos votos totais de um estado. O estudo se fundamenta em pesquisa de caráter exploratório-explicativa, com abordagem quantitativa aplicada a dados eleitorais das votações dos 513 candidatos eleitos para deputado federal nos 26 estados e no Distrito Federal, nas eleições de 2014. Os índices N, G, Moran e M foram testados usando a agregação espacial do município, sendo que o índice N também foi testado empregando a agregação das zonas eleitorais, e o índice M, utilizando um mapa de agregação espacial misto, combinando municípios e as zonas eleitorais dos grandes municípios.

A partir desses dados, foi possível compor um quadro comparativo com os coeficientes dos indicadores de todos os estados. Os resultados indicam que o índice N por município foi o que apresentou menor capacidade de traduzir a distribuição eleitoral dos candidatos. Os índices G e N por zona eleitoral conseguiram espelhar melhor a realidade das votações em seus coeficientes, especialmente o índice N por zona eleitoral que, por utilizar uma agregação eleitoral com menor variação de tamanho, obteve melhores resultados do que aqueles apresentados pelo N por município. Os coeficientes calculados pelo I de Moran não foram significativos. Já o índice M, em comparação com os demais indicadores, apresentou os resultados mais satisfatórios para a representação das votações dos deputados eleitos, em especial, nos cálculos usando a agregação mista de votos. Por fim, o índice M foi testado utilizando as seções de votação como base de agregação eleitoral em pequenos estados. Essa agregação eleitoral foi mais eficiente em refletir a distribuição eleitoral quando comparada com as demais agregações em estados de mesma magnitude.

## GeografiaEleitoral.R

Script que efetua a leitura dos arquivos de votação e de dados geoespaciais *(shapefiles)*, prepara as variáveis para processamento e calcula os índices de medição da geografis eleitoral. Para a execução do script é necessário descompactar os arquivos de votação e de dados geoespaciais.

## MunicipiosShapefiles.zip.###

Arquivos vetoriais *shapefile*, compactados no formato "zip", contendo as malhas territoriais municipais das unidades da federação e, 2014, incluindo a divisão das mesoregiões, microregiões, municípios e setores censitários. Os arquivos deste repositório foram extraídos da página do [IBGE](https://downloads.ibge.gov.br/downloads_geociencias.htm) e devem ser descompactados antes de ser executado o script "GeografiaEleitoral.R".

*Shapefile* é um formato popular de arquivo contendo dados geoespaciais em forma de vetor, usado por Sistemas de Informações Geográficas.

## MunicipioZona_votacao.zip.###

Arquivos compactados no formato "zip", extraídos do [Repositório de Dados Eleitorais do TSE](http://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais), contendo a votação nominal dos candidatos por município e zona eleitoral, em cada unidade da federação, referentes às eleições de 2014. Devem ser descompactados antes de ser executado o script "GeografiaEleitoral.R".

## Indices_Brasil.csv

Arquivo resultante da execução do script "GeografiaEleitoral.R", contendo a relação dos deputados federais eleitos em 2014 e os respectivos índices de medição da geografia eleitoral.

## Indices_Medios_estado.csv

Arquivo resultante da execução do script "GeografiaEleitoral.R", contendo o valor médio dos índices de medição da geografia eleitoral por unidade da federação.

## Dissertação

Íntegra da dissertação de mestrado [ANÁLISE COMPARADA DOS MODELOS DE MEDIÇÃO DA GEOGRAFIA ELEITORAL: estudo da efetividade por meio de abordagem quantitativa](https://sucupira.capes.gov.br//sucupira/public/consultas/coleta/trabalhoConclusao/viewTrabalhoConclusao.jsf?popup=true&id_trabalho=7623973) disponível na plataforma Sucupira.





