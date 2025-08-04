# SEC 4th-Down Decision Bot

## Purpose
This repository delivers real-time 4th-down advice for every SEC football program.

* Predicts coach choice (Go / Punt / Field Goal).  
* Quantifies win-probability (WP) impact.  
* Displays 50 nearest historical plays for context.  
* All heavy compute runs on Google Cloud Run; the Shiny UI is purely front-end.

---

## Repository Layout

```
/api/            Plumber sources + Dockerfile  
/models/         *.rds coach classifiers + WP model  
/similarity/     *.rds k-nearest-neighbour objects  
/data/           Optional cached PBP frames per team  
/shiny/          Front-end (ui.R, server.R, assets)  
/scripts/        Training + local helpers  
README.md
```

---

## Data Pipeline

| Stage   | Detail                                                                                                 |
|---------|--------------------------------------------------------------------------------------------------------|
| Ingest  | `cfbfastR::load_cfb_pbp()` for seasons 2016-2024.                                                      |
| Filter  | Keep `down == 4`; drop plays missing field position, clock, or score data.                             |
| Mapping | A “play-caller plan” ties each SEC coach to prior schools so relevant historical plays stay in sample. |
| Output  | 17 datasets: one global (`pbp_4th_all`) + 16 team-specific frames (saved in `/data`).                  |

Derived flags:  
`goal_to_go = distance == yards_to_goal` (0 / 1)  
`under_two = clock.minutes < 2` (0 / 1)  
`fg_distance = yards_to_goal + 17`

---

## Model Training

### 4.1 Coach-Decision Classifiers
* Algorithm `xgboost` (400 trees, depth 6, η = 0.10).  
* Pipeline recipe → dummy-code → impute → normalise → SMOTE → model.  
* Target `decision ∈ {Go for it, Punt, Field Goal}`.  
* Saved as `models/<team_slug>_model.rds`.

### 4.2 Global Win-Probability Regressor
* Algorithm Random-forest (`ranger`, 500 trees).  
* Target `wp_before` (pre-snap WP).  
* Performance RMSE ≈ 0.0325, MAE ≈ 0.0149, R² ≈ 0.991.  
* Saved as `models/wp_model.rds` (butchered to shrink); downloaded lazily by API.

### 4.3 Play-Similarity Objects
Each `/similarity/<team>_sim.rds` stores:  
* 10-column numeric matrix (distance, yards_to_goal, clock, etc.).  
* Full tibble of raw plays for neighbour look-ups.

---

## Model Evaluation

### 5.1 Coach-Decision Metrics  

| Team                | Accuracy | Kappa |
|---------------------|---------:|------:|
| Alabama             | 0.812 | 0.697 |
| Arkansas            | 0.914 | 0.860 |
| Auburn              | 0.824 | 0.711 |
| Florida             | 0.849 | 0.755 |
| Georgia             | 0.820 | 0.718 |
| Kentucky            | 0.857 | 0.763 |
| LSU                 | 0.884 | 0.811 |
| Mississippi State   | 0.853 | 0.759 |
| Missouri            | 0.862 | 0.773 |
| Ole Miss            | 0.833 | 0.738 |
| Oklahoma            | 0.849 | 0.763 |
| South Carolina      | 0.806 | 0.663 |
| Tennessee           | 0.844 | 0.745 |
| Texas               | 0.844 | 0.751 |
| Texas A&M           | 0.832 | 0.710 |
| Vanderbilt          | 0.833 | 0.707 |

*(Regression metrics do not apply to these classifiers.)*

### 5.2 Win-Probability Metrics  

| Metric | Value |
|--------|------:|
| RMSE   | 0.0325 |
| MAE    | 0.0149 |
| R²     | 0.991 |

---

## 6 APIs

**Files** `/api/plumber.R`, `/api/coach_api.R`, `/api/wp_api.R`

| Endpoint            | Description                       |
|---------------------|-----------------------------------|
| `/predict_decision` | Returns coach’s historical choice |
| `/wp`               | Returns predicted WP              |

Both run in one Plumber server (port 8080).  
Models are memoised so each container loads them only once.

---

## Container & Cloud Run Deployment

1. **Build image**

    ```
    docker build -t sec-4thdown-api ./api
    ```

2. **Push to Google Container Registry**

    ```
    docker tag sec-4thdown-api gcr.io/<PROJECT_ID>/sec-4thdown-api
    docker push gcr.io/<PROJECT_ID>/sec-4thdown-api
    ```

3. **Deploy to Cloud Run**

    ```
    gcloud run deploy sec-4thdown-api \
      --image gcr.io/<PROJECT_ID>/sec-4thdown-api \
      --platform managed \
      --region us-central1 \
      --allow-unauthenticated
    ```

WP model is fetched from GCS at runtime to keep the container slim.

---

## Shiny Front-End

* Located in `/shiny`.  
* Accepts user-defined situation, calls both APIs, and draws:  
  * Recommended play from global model.  
  * Coach decision from team model.  
  * Table of WP changes for five scenarios.  
  * Field graphic and team-branded UI.  
* Also presents 50 nearest historical plays using pre-saved similarity object.

---

## Retraining & Extension

* **Add seasons** Run `scripts/sec_model_builder.R`; new `.rds` overwrite old.  
* **Add teams** Extend play-caller plan and rerun training script.  
* **Alternate algorithms** Swap SMOTE strategy or tune `xgboost` grid.

---

## Local Development

* Build everything:

    ```r
    source("scripts/sec_model_builder.R")
    ```

* Launch API offline:

    ```r
    source("scripts/plumber_start.R")
    ```

* Run Shiny:

    ```
    R -e "shiny::runApp('shiny', host = '0.0.0.0', port = 3838)"
    ```

---

## Key Points

* Coach classifiers hit 81–91 % accuracy by team.  
* Global WP model shows RMSE ≈ 0.03 and R² ≈ 0.99.  
* Cloud Run handles scaling while Shiny stays lightweight.  
* Models, similarity objects, and Docker image are fully reproducible.

Copy this file into your repo as `README.md`; GitHub will render headings, tables, and indented code blocks correctly.
