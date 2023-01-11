## [Shiny App](https://sgbstats.shinyapps.io/rrsapp/)


### Equation of the continuous model

$PI=1.250\log⁡(Creatinine)-0.0167Normalpercent+0.616IFTA$

$\log⁡(H_0(t))=-9.532-0.643t^{-0.1}-0.384t^{0.1}\log(t)$

$S_0(t)=\exp(-\exp(\log⁡(H_0(t)))=\mathbb{P}(T>t|PI=0)$

$S(t,PI)=\mathbb{P}(T>t│PI)=S_0(t)^{\exp(PI)}$

### Risk points

| Variable    | β   | HR (CI)                    | Points |
|-------------|------|------------------------|--------|
| Creatinine (μmol) |      |                       |        |
| C0: < 250   | Ref  |                        | 0      |
| C1: 250 – 450 | 0.661 | 1.94 (1.30-2.89) | 4      |
| C2: > 450   | 1.886 | 6.59 (4.59-9.45) | 11     |
| Normal Glomeruli (%) |      |                       |        |
| N0: > 25    | Ref  |                        | 0      |
| N1: 10 - 25 | 0.65  | 1.92 (1.30-2.82)  | 4      |
| N2: < 10    | 1.199 | 3.32 (2.35-4.69)  | 7      |
| IFTA        |      |                       |        |
| T0: < 25    | Ref  |                        | 0      |
| T1: ≥ 25    | 0.527 | 1.69 (1.28-2.26)  | 3      |

### Groupings

| Group     | Points   |
|-----------|----------|
| Low       | 0 – 4    |
| Moderate  | 5 – 11   |
| High      | 12 – 18  |
| Very high | 21       |
