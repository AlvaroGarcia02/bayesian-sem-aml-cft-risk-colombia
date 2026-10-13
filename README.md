# Bayesian SEM for Latent AML/CFT Risk in Colombian Municipalities

**Modelo de ecuaciones estructurales bayesiano para la predicción y clasificación del riesgo latente de lavado de activos y financiación del terrorismo en los municipios de Colombia**

## Overview

Money laundering and terrorist financing (AML/CFT) risk cannot be observed directly; only indirect indicators are available. This project develops and compares three Bayesian structural equation models (SEM) to estimate a latent risk measure for each of Colombia's 1,122 municipalities, and uses its posterior distribution to classify municipalities and departments into low, medium and high relative risk.

## Models

| Model | Description |
|---|---|
| M1 · MLR-LAFT | Latent risk model with independent errors |
| M2 · MLR-LAFT-ED | + department random effects |
| M3 · MLR-LAFT-EED | + spatial department effects (ICAR) |

## Methodology

- **Structural model:** latent risk explained by thematic covariates (structural, political, smuggling and AML-susceptible economic sectors).
- **Measurement model:** four observed indicators (terrorism, armed conflict, crime, drug trafficking) modeled with zero-inflated Gamma distributions.
- **Spatial dependence:** intrinsic conditional autoregressive (ICAR) prior over departments.
- **Estimation:** MCMC samplers programmed from scratch in R, combining Gibbs sampling with Metropolis–Hastings steps. 110,000 iterations, 10,000 burn-in, thinning of 10.
- **Model comparison:** convergence diagnostics, effective sample size, DIC, WAIC, MSE, MAE, R² and posterior predictive checks.

## Results

| Model | DIC |
|---|---|
| M1 · MLR-LAFT | 890.69 |
| M2 · MLR-LAFT-ED | 722.76 |
| M3 · MLR-LAFT-EED | 722.18 |

Full report (Spanish): [docs/informe_sem_bayesiano_laft.pdf](docs/informe_sem_bayesiano_laft.pdf)

## Repository structure

| Location | Content |
|---|---|
| `R/` | Full R script: data preparation, EDA, maps, Gibbs samplers, diagnostics, inference and classification |
| `data/` | Transformed LAFT dataset and department shapefile |
| `docs/` | Full report |
| `figures/` | Figures |
| [Release v1.0](../../releases/tag/v1.0) | MCMC chains (~210 MB each) and municipal shapefile |

## How to reproduce

1. Download this repository (**Code → Download ZIP**) and unzip it.
2. Download the three chains and `SHP_MGN2018_INTGRD_MPIO.zip` from [Release v1.0](../../releases/tag/v1.0).
3. Place the three chains in the root folder, and unzip both shapefile zips inside `data/`.
4. In R, set the working directory to the root folder and install the packages:
```r
   install.packages(c("readxl", "Hmisc", "ggplot2", "RColorBrewer", "sf", "dplyr",
                      "viridis", "corrplot", "tictoc", "data.table", "MASS",
                      "Matrix", "truncnorm", "coda"))
```
5. Run `R/sem_bayesiano_laft.R`. Running the samplers is optional (several hours each); seeds are fixed, so results match the provided chains.

## Data sources

- Muñoz Chamorro, D. V. (2024). *Desarrollo de modelo de aprendizaje estadístico para la identificación del riesgo de lavado de activos y financiación del terrorismo*. M.Sc. thesis, Universidad Nacional de Colombia. https://repositorio.unal.edu.co/handle/unal/87731 (variables transformed as described in Appendix D of the report).
- Marco Geoestadístico Nacional 2018, DANE.

## Authors

M. Paula Arévalo F. · Álvaro A. García G.

Case study for the course *Estadística Bayesiana*, Department of Statistics, Universidad Nacional de Colombia (2026-1), supervised by Prof. Juan Camilo Sosa Martínez.

---

## Resumen en español

Desarrollo y comparación de tres modelos bayesianos de ecuaciones estructurales para estimar el riesgo latente de lavado de activos y financiación del terrorismo (LA/FT) en los municipios de Colombia. Se incorporan covariables temáticas, distribuciones Gamma cero-infladas para modelar indicadores observados, efectos departamentales y dependencia espacial mediante estructuras ICAR. La estimación se realiza con algoritmos MCMC que combinan muestreo de Gibbs y métodos de Metropolis-Hastings. Finalmente, se emplean las estimaciones posteriores para caracterizar y clasificar los municipios y departamentos en niveles relativos de riesgo.
