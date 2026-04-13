# New Hampshire Gazette Thesis Code

This repository contains the code used for my master's thesis research on the language of politics in the *New-Hampshire Gazette* (1756–1783), using computational text analysis and word embeddings. The project focuses especially on the word **"tyranny"**, examining both its changing frequency and its shifting semantic associations across time.

## Project Overview

This repository supports a study of how political language changes over time in early American newspapers. Using OCR text from the *New-Hampshire Gazette*, the code in this repo is used to:

- measure yearly and monthly frequencies of key political terms
- compare normalized word frequencies across time
- examine relationships between words such as *tyranny*, *liberty*, *king*, *parliament*, *crown*, *ministry*, *popery*, and *Cato*
- build and compare word embedding models across historical periods
- analyze semantic drift through aligned vector spaces
- visualize semantic neighborhoods using bar charts, PCA plots, dendrograms, and network graphs

The repository reflects a working thesis codebase that was developed iteratively during research and then cleaned for reproducibility.

## Research Focus

The central research question concerns how the meaning and rhetorical use of **"tyranny"** evolved in the *New-Hampshire Gazette* between 1756 and 1783.

The code is used to investigate:

- when the word appears more or less frequently
- whether its usage changes across major political periods
- how closely it is semantically associated with other political or religious terms
- how its broader semantic field changes before, during, and after the imperial crisis and the American Revolution

## Repository Structure

The repository is organized around three main analytical tasks:

### 1. Frequency and temporal analysis
Scripts in this part of the project:

- read and clean the OCR corpus
- tokenize the newspaper text
- count word frequencies by year and month
- calculate normalized frequencies per million words
- generate plots for terms such as:
  - tyranny
  - liberty
  - Cato
  - popery
  - standing army / standing armies
- generate ratio plots such as liberty-to-tyranny over time
- create simple animations of word frequency over time

### 2. Period word embedding comparison
Scripts in this section:

- load pre-trained word embedding models for different historical periods
- align embedding spaces using shared vocabulary and SVD-based rotation
- compare cosine similarity for selected terms across periods
- measure semantic drift for *tyranny*
- compare *tyranny* to words such as:
  - parliament
  - king
  - crown
  - ministry

The main periods used in the repo are:

- **1756–1764**
- **1765–1776**
- **1777–1783**

### 3. Semantic neighborhood exploration
Scripts in this section:

- identify nearest semantic neighbors of *tyranny*
- visualize nearest neighbors with bar charts
- project semantic neighborhoods with PCA
- build dendrograms of neighbors and neighbors-of-neighbors
- build network graphs of semantic fields using `igraph` and `ggraph`

## Data

The project uses OCR text from the *New-Hampshire Gazette* from **Chronicling America**.

The main corpus file used in the frequency analysis is:

- `NHgazette1756-1783.csv`

Expected core columns include:

- `sequence`
- `date`
- `ocr_eng`

The code assumes that dates are formatted so the year and month can be extracted with substring operations.

## Folder Layout

A typical layout for the repository is:

```text
.
├── NHgazette1756-1783.csv
├── period_corpus/
│   └── vectorsv2.bin
├── period_models/
│   ├── 1756-1764_vectors.bin
│   ├── 1765-1776_vectors.bin
│   └── 1777-1783_vectors.bin
├── nhgazettevisualizations/
└── scripts/