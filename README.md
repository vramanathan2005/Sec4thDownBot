# SEC 4th-Down Decision Bot

## Purpose
Real-time 4th-down advice for every SEC football program:

* Predicts coach call (Go / Punt / Field Goal).  
* Quantifies win-probability (WP) impact.  
* Shows 50 nearest historical plays.  
* Cloud-hosted APIs (Google Cloud Run) + Shiny front-end.

---

## Repository Layout

```
/api/            Plumber sources + Dockerfile  
/models/         *.rds coach classifiers + WP model  
/similarity/     *.rds k-nearest-neighbour objects  
/data/           Cached play-by-play (optional)  
/shiny/          Front-end (ui.R, server.R, assets)  
/scripts/        Training + local helpers  
README.md
```

---

## Data Pipeline

* **Seasons loaded** 2019 – 2024 via `cfbfastR::load_cfb_pbp()`.  
* **Filter** `down == 4`; drop plays lacking distance, field-pos, score, or clock.  
* **Derived flags** `goal_to_go`, `under_two`, `fg_distance`.  
* **Outputs** global set `pbp_4th_all` and 16 team-specific frames.

### Coach-Specific Sampling Rules
Each SEC model uses plays from the head-coach / OC’s career stops so the algorithm learns that coach’s tendencies, even before arriving at the current school.

| SEC Team | Off-Field Schools Used | Seasons Included |
|----------|-----------------------|------------------|
| Alabama | Fresno State, Washington, Alabama | 2019-2021, 2022-2023, 2024 |
| Arkansas | Missouri State, Texas A&M, Arkansas | 2020-2022, 2023, 2024 |
| Auburn | Liberty, Auburn | 2019-2022, 2023-2024 |
| Florida | Louisiana, Florida | 2019-2021, 2022-2024 |
| Georgia | Colorado State, South Carolina, Auburn, Georgia | 2019, 2020, 2021, 2022-2024 |
| Kentucky | Washington, Missouri, Boise State, Kentucky | 2019, 2020-2022, 2023, 2024 |
| LSU | Louisiana Tech, LSU | 2019-2021, 2022-2024 |
| Mississippi State | UCF, Ole Miss, Oklahoma, Mississippi State | 2019, 2020-2021, 2022-2023, 2024 |
| Missouri | Fresno State, Missouri | 2019-2022, 2023-2024 |
| Ole Miss | Florida Atlantic, Ole Miss | 2019, 2020-2024 |
| Oklahoma | Houston Baptist, Western Kentucky, Washington State | 2019, 2021-2022, 2023-2024 |
| South Carolina | Oklahoma, South Carolina | 2019, 2020-2024 |
| Tennessee | UCF, Tennessee | 2019-2020, 2021-2024 |
| Texas | Alabama, Texas | 2019-2020, 2021-2024 |
| Texas A&M | Kansas State, Texas A&M | 2019-2023, 2024 |
| Vanderbilt | Pittsburg State, TCU, New Mexico State, Vanderbilt | 2019, 2021, 2022-2023, 2024 |

---

## Model Training

### Coach-Decision Classifiers
* **Algorithm** `xgboost` (400 trees, depth 6, η 0.10).  
* **Features** distance, yards-to-goal, clock, period, score, timeouts, `goal_to_go`, `under_two`, `fg_distance`.  
* **Pipeline** dummy-code → impute → normalise → SMOTE → model.  
* **Output** `models/<team>_model.rds`.

### Global Win-Probability Regressor
* **Algorithm** Random-forest (`ranger`, 500 trees).  
* **Target** `wp_before`.  
* **Output** `models/wp_model.rds` (butchered and hosted on GCS).  
* **Performance** RMSE 0.0325, MAE 0.0149, R² 0.991.

### Play-Similarity Objects
10-column numeric matrix + raw tibble per team; saved in `/similarity/`.

---

## Model Evaluation

### Coach-Decision Metrics (test sets)

| Team | Acc. | κ |
|------|-----:|--:|
| Alabama | 0.812 | 0.697 |
| Arkansas | 0.914 | 0.860 |
| Auburn | 0.824 | 0.711 |
| Florida | 0.849 | 0.755 |
| Georgia | 0.820 | 0.718 |
| Kentucky | 0.857 | 0.763 |
| LSU | 0.884 | 0.811 |
| Mississippi St. | 0.853 | 0.759 |
| Missouri | 0.862 | 0.773 |
| Ole Miss | 0.833 | 0.738 |
| Oklahoma | 0.849 | 0.763 |
| South Carolina | 0.806 | 0.663 |
| Tennessee | 0.844 | 0.745 |
| Texas | 0.844 | 0.751 |
| Texas A&M | 0.832 | 0.710 |
| Vanderbilt | 0.833 | 0.707 |

### Win-Probability Metrics (global)

| RMSE | MAE | R² |
|-----:|----:|---:|
| 0.0325 | 0.0149 | 0.991 |

---

## APIs

* `/predict_decision` coach classifier output (string).  
* `/wp` win-probability (numeric).  
* Both served by one Plumber app (port 8080) and memoise their models per container.

---

## Container & Cloud-Run Deployment

```
docker build -t sec-4thdown-api ./api
docker tag sec-4thdown-api gcr.io/<PROJECT_ID>/sec-4thdown-api
docker push gcr.io/<PROJECT_ID>/sec-4thdown-api

gcloud run deploy sec-4thdown-api \
  --image gcr.io/<PROJECT_ID>/sec-4thdown-api \
  --platform managed \
  --region us-central1 \
  --allow-unauthenticated
```

WP model downloads from GCS at runtime; all other artefacts copied into the image.

---

## Shiny Front-End

* Located in `/shiny`.  
* Queries both endpoints and renders:  
  * Global recommendation.  
  * Coach prediction.  
  * WP table for Go/FG/Punt scenarios.  
  * Field graphic and SEC-themed UI.  
  * 50-play neighbour summary & details.

---

## Retraining & Extension

* **Add seasons** Run `scripts/sec_model_builder.R`; new `.rds` overwrite old.  
* **Add teams** Extend play-caller plan; rerun builder.  
* **Experiment** Swap SMOTE or tune `xgboost` grid.

---

## Local Development

```
# Build or rebuild everything
source("scripts/sec_model_builder.R")

# Launch API locally
source("scripts/plumber_start.R")   # default port 8080

# Run Shiny
R -e "shiny::runApp('shiny', host = '0.0.0.0', port = 3838)"
```

---

## Key Points

* Coach classifiers: 0.81–0.91 accuracy across teams.  
* Global WP: RMSE ≈ 0.03, R² ≈ 0.99.  
* Cloud Run handles scaling; Shiny remains light.  
* All artefacts and infrastructure are reproducible with provided scripts.
