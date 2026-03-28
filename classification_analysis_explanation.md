# Classification Table and Cut-Off Analysis Explanation

## Overview

This document explains the classification table and optimal cut-off point analysis added to `deliverable3.R` for the multinomial logistic regression model.

---

## 1. Classification Table (Confusion Matrix)

### Purpose
The classification table evaluates how well the model predicts the **training data** by comparing predicted classes against actual observed classes.

### Code
```r
# Get predicted probabilities for all classes
predicted_probs <- predict(final_model, type = "probs")

# Get predicted classes (default: highest probability wins)
predicted_class <- predict(final_model, type = "class")

# Classification table (confusion matrix)
conf_matrix <- confusionMatrix(predicted_class, transformedDataFrame$target_employment)
conf_matrix
```

### What It Does
- `predict(final_model, type = "probs")` returns a matrix with probabilities for each class (LOW, MEDIUM, HIGH)
- `predict(final_model, type = "class")` assigns each observation to the class with the highest probability
- `confusionMatrix()` from the `caret` package creates the confusion matrix and computes metrics

### Output Metrics
| Metric | Description |
|--------|-------------|
| **Accuracy** | Overall proportion of correct predictions |
| **Sensitivity** | True positive rate (correctly predicted positives / actual positives) |
| **Specificity** | True negative rate (correctly predicted negatives / actual negatives) |

### Interpretation
- **High accuracy** → Model predicts correctly most of the time
- **High sensitivity** → Model rarely misses positive cases
- **High specificity** → Model rarely misclassifies negatives as positives

---

## 2. ROC Curve Analysis

### Purpose
ROC (Receiver Operating Characteristic) curves visualize the trade-off between sensitivity and specificity across all possible cut-off points.

### Code
```r
library(pROC)

# ROC for MEDIUM class (One-vs-Rest)
roc_medium <- roc(as.numeric(transformedDataFrame$target_employment == "MEDIUM"),
                  predicted_probs[, "MEDIUM"],
                  plot = TRUE, legacy.axes = TRUE, print.auc = TRUE, col = "blue")

# ROC for HIGH class (One-vs-Rest)
roc_high <- roc(as.numeric(transformedDataFrame$target_employment == "HIGH"),
                predicted_probs[, "HIGH"],
                plot = TRUE, legacy.axes = TRUE, print.auc = TRUE, col = "red", add = TRUE)
```

### What It Does
- Uses **One-vs-Rest** approach: treats each class as "positive" and all others as "negative"
- `as.numeric(transformedDataFrame$target_employment == "MEDIUM")` creates a binary vector (1 = MEDIUM, 0 = not MEDIUM)
- Plots Sensitivity (y-axis) vs 1-Specificity (x-axis)
- **AUC (Area Under Curve)**: Higher AUC = better discrimination ability
  - AUC = 0.5 → No better than random guessing
  - AUC = 1.0 → Perfect classification

### Interpretation
- **AUC for MEDIUM**: How well the model distinguishes MEDIUM from LOW+HIGH
- **AUC for HIGH**: How well the model distinguishes HIGH from LOW+MEDIUM
- Curve closer to top-left corner = better model

---

## 3. Optimal Cut-Off Point Selection

### Purpose
The default prediction rule (highest probability wins) may not always be optimal. Finding the optimal cut-off helps balance sensitivity and specificity.

### Method: Youden's Index
```r
# Youden's Index = Sensitivity + Specificity - 1
# Maximizes the sum of sensitivity and specificity

# MEDIUM class
youden_medium <- roc_medium$sensitivities + roc_medium$specificities - 1
optimal_idx_medium <- which.max(youden_medium)
optimal_cutoff_medium <- roc_medium$thresholds[optimal_idx_medium]

# HIGH class
youden_high <- roc_high$sensitivities + roc_high$specificities - 1
optimal_idx_high <- which.max(youden_high)
optimal_cutoff_high <- roc_high$thresholds[optimal_idx_high]
```

### What It Does
- **Youden's Index** finds the point where sensitivity + specificity is maximized
- Geometrically: point on ROC curve farthest from the diagonal (random guessing line)
- Returns the probability threshold that best balances true positives and true negatives

### Interpretation
| Cut-Off Value | Interpretation |
|---------------|----------------|
| **Low cutoff** (~0.1-0.2) | Easier to predict positive → Higher sensitivity, lower specificity |
| **High cutoff** (~0.6-0.8) | Harder to predict positive → Lower sensitivity, higher specificity |
| **Optimal cutoff** | Best balance between sensitivity and specificity |

---

## 4. Visualizing Cut-Off Points

### Purpose
Plot sensitivity and specificity curves against cut-off points to visualize where they intersect.

### Code
```r
plot(roc_medium$thresholds, roc_medium$sensitivities, type = "l",
     xlab = "Cut-off Point", ylab = "Sensitivity/Specificity", bty = "n")
lines(roc_medium$thresholds, roc_medium$specificities)
abline(v = optimal_cutoff_medium, col = "gray", lty = 2)
legend("topright", legend = c("Sensitivity", "Specificity"),
       col = c("black", "black"), lty = c(1, 1))
```

### Interpretation
- **Intersection point**: Where sensitivity ≈ specificity
- **Vertical line**: Optimal cut-off (Youden's index maximum)
- Steeper curves = better model discrimination

---

## 5. Required Libraries

```r
library(caret)    # For confusionMatrix()
library(pROC)     # For ROC curve analysis
```

Install if needed:
```r
install.packages("caret")
install.packages("pROC")
```

---

## Summary of Results to Report

After running the analysis, report:

1. **Classification Table Metrics**
   - Overall Accuracy: ___%
   - Sensitivity (LOW): ___%
   - Sensitivity (MEDIUM): ___%
   - Sensitivity (HIGH): ___%

2. **ROC Curve AUC Values**
   - AUC (MEDIUM vs Rest): ___
   - AUC (HIGH vs Rest): ___

3. **Optimal Cut-Off Points**
   - MEDIUM class: ___ (Youden's Index)
   - HIGH class: ___ (Youden's Index)

4. **Interpretation**
   - Does the model classify well? (accuracy > 70%?)
   - Is there a class that's harder to predict? (low sensitivity?)
   - Do optimal cut-offs differ significantly from default (1/3 for 3 classes)?

---

## References

- **Youden's Index**: Youden WJ (1950). "Index for rating diagnostic tests". *Cancer*, 3(1), 32-35.
- **ROC Analysis**: Zweig MH, Campbell G (1993). "Receiver-operating characteristic (ROC) plots". *Clinical Chemistry*, 39(4), 561-577.
- **caret package**: Kuhn M (2008). "Building Predictive Models in R Using the caret Package". *Journal of Statistical Software*, 28(5), 1-26.

---

## Appendix A: How Predicted Classes are Calculated

### Step-by-Step Process in Multinomial Logistic Regression

When you call `predict(final_model, type = "class")`, R performs the following steps:

### Step 1: Compute Linear Predictors (Logits) for Each Class

For each observation and each class (except the baseline), the model computes a **logit**:

```
logit(MEDIUM) = β₀(M) + β₁(M)·x₁ + β₂(M)·x₂ + ... + βₖ(M)·xₖ
logit(HIGH)   = β₀(H) + β₁(H)·x₁ + β₂(H)·x₂ + ... + βₖ(H)·xₖ
```

Where:
- `LOW` is the **baseline/reference class** (you set it with `relevel()`)
- The logit for `LOW` is implicitly **0** (baseline)

### Step 2: Convert Logits to Probabilities (Softmax Function)

The logits are transformed to probabilities using the **softmax function**:

```
                exp(logit(MEDIUM))
P(MEDIUM) = ─────────────────────────────────────────
            1 + exp(logit(MEDIUM)) + exp(logit(HIGH))

                exp(logit(HIGH))
P(HIGH)   = ─────────────────────────────────────────
            1 + exp(logit(MEDIUM)) + exp(logit(HIGH))

                1
P(LOW)    = ─────────────────────────────────────────
            1 + exp(logit(MEDIUM)) + exp(logit(HIGH))
```

**Key property:** All three probabilities sum to 1.

### Step 3: Assign Class with Highest Probability

```r
# For each observation (row), find which column has max probability
predicted_class = argmax(P(LOW), P(MEDIUM), P(HIGH))
```

**Example:**

| Observation | P(LOW) | P(MEDIUM) | P(HIGH) | Predicted Class |
|-------------|--------|-----------|---------|-----------------|
| 1           | 0.15   | 0.35      | 0.50    | **HIGH**        |
| 2           | 0.60   | 0.25      | 0.15    | **LOW**         |
| 3           | 0.20   | 0.55      | 0.25    | **MEDIUM**      |

### Visual Representation

```
                    SOFTMAX
    ┌─────────────────────────────────────┐
    │  logit(MEDIUM)  ────►  P(MEDIUM)    │
    │  logit(HIGH)    ────►  P(HIGH)      │
    │  0 (baseline)   ────►  P(LOW)       │
    └─────────────────────────────────────┘
                        │
                        ▼
              ┌───────────────────┐
              │  argmax()         │
              │  (highest prob)   │
              └───────────────────┘
                        │
                        ▼
              PREDICTED CLASS
```

### In Your Code

```r
# Get probabilities (Step 1 + Step 2)
predicted_probs <- predict(final_model, type = "probs")
# Returns a matrix with columns: LOW, MEDIUM, HIGH

# Get predicted class (Step 3)
predicted_class <- predict(final_model, type = "class")
# Returns: "LOW", "MEDIUM", or "HIGH" for each observation

# Equivalent manual calculation:
predicted_class_manual <- apply(predicted_probs, 1, function(row) {
  colnames(predicted_probs)[which.max(row)]
})
```

### Why Use "Highest Probability" Instead of Cut-Offs?

| Binary Logistic Regression (2 classes) | Multinomial Logistic Regression (3+ classes) |
|----------------------------------------|----------------------------------------------|
| ONE probability: P(Y=1) | MULTIPLE probabilities that sum to 1 |
| Compare to single cut-off (e.g., 0.5) | Probabilities are **interdependent** |
| Simple threshold decision | If P(LOW) increases, P(MEDIUM) and/or P(HIGH) must decrease |
| | **Highest probability** is the natural extension |

**Key Insight:** In multinomial models, cut-offs are useful for **diagnostics** (understanding model confidence), but the "highest probability" rule is the standard for **prediction** because:

1. Probabilities must sum to 1 (they compete with each other)
2. A single threshold doesn't make sense with 3+ classes
3. The softmax function already handles the relative comparisons optimally

### Viewing Your Model's Coefficients

To see the actual β coefficients used in Step 1:

```r
coef(final_model)
```

This displays the coefficients for MEDIUM and HIGH (LOW is the baseline/reference).
