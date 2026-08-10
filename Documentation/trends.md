<!-- File: trends.md -->
# 📊 Trends Tab

## Table of Contents
1. [Overview](#overview)
2. [How to Interpret the Figure](#how-to-interpret-the-figure)
3. [Points of Caution](#points-of-caution)
4. [Data Filters](#data-filters)
5. [Plot Controls](#plot-controls)
6. [Legend](#legend)
7. [Output](#output)
8. [Feedback](#feedback)

---

## 🧭 Overview<a name="overview"></a>

The **Trends** tab provides a time-based view of your AMR data through two complementary metrics:

1. **% Susceptible** — Shows the percentage of bacterial isolates interpreted as Susceptible (S) for selected antimicrobials over time
2. **% Organism Prevalence** — Shows what percentage of all isolates were a specific organism at each time point

Date information is extracted from either a single `'Date'` column or from separate `'Year'`, `'Month'`, and/or `'Day'` columns in your dataset. Hovering over any point on the plot reveals additional details, including sample sizes and percentages.

### Choosing Your Metric

- **Use % Susceptible** when analyzing antimicrobial resistance patterns and tracking how susceptibility to specific drugs changes over time
- **Use % Organism Prevalence** when tracking which organisms are most common in your population, independent of their resistance profiles

### Data Sources for Prevalence Analysis

When analyzing **% Organism Prevalence**, you can choose between two data sources:

- **AST isolates only** — Uses only isolates that underwent antimicrobial susceptibility testing
- **All cultures** — Includes all cultured isolates, even those that were never tested for susceptibility (requires opting in during data import)

Using "All cultures" provides a more complete picture of organism prevalence, since not all cultured isolates undergo AST.

> ⚠️ **Note:** For **% Susceptible** analysis, to maintain compliance with [CLSI M39](https://clsi.org/shop/standards/m39/) guidelines, time points with fewer than 30 observations are automatically **rolled forward** into the next time interval until the minimum sample size is met. For **% Organism Prevalence**, data are binned by fixed time periods (Month/Quarter/Year) to ensure consistent denominators across all organisms.

---

## 📖 How to Interpret the Figure<a name="how-to-interpret-the-figure"></a>

Time series plots show how patterns evolve over time. The interpretation depends on which metric you've selected:

### % Susceptible Plots

Each point represents the **percentage of isolates interpreted as Susceptible (S)** for a given antimicrobial during a specific time period. These plots are useful for:

- Identifying **long-term resistance trends** (e.g., increasing or decreasing susceptibility)
- Spotting **sudden changes or shifts**, which may suggest emerging resistance or changes in testing practices
- Comparing patterns across antimicrobials, microorganisms, or regions

### % Organism Prevalence Plots

Each point represents the **percentage of all isolates that were a specific organism** during a fixed time period (month, quarter, or year). These plots are useful for:

- Tracking **which organisms are most common** in your population over time
- Identifying **seasonal patterns** in organism prevalence
- Detecting **emerging pathogens** or shifts in the microbial landscape
- Understanding the **denominator** for your resistance analyses (e.g., "resistance in E. coli is rising, but E. coli itself is becoming less common")

> 💡 **Tip:** When multiple organisms are selected in prevalence mode, each is shown as a separate line. The percentages represent the proportion of *all isolates* that were each organism, so they can sum to more than 100% if you're viewing all organisms.

---

## ⚠️ Points of Caution<a name="points-of-caution"></a>

- **Small Sample Sizes**  
  Time points with low numbers of isolates may produce unstable or misleading percentages.

- **Changes in Testing Behavior**  
  Apparent shifts in resistance may reflect changes in laboratory testing practices, submission patterns, or diagnostic protocols—not necessarily true biological change.

- **Aggregation Effects**  
  Trends may differ depending on how the data are grouped (e.g., by species, region, or site). Always consider applied filters when interpreting the plot.

> Remember to always interpret patterns and trends in the context of your data’s scope, sample sizes, and any applied filters.

---

## 🧰 Data Filters<a name="data-filters"></a>

Customize the time series plot using the **Filters** panel (click the *pencil* icon ✏️ in the top right corner of the panel to add/remove fields).

| Default Filter          | Purpose                                                      |
|-------------------------|--------------------------------------------------------------|
| **Microorganism** | Select the microorganism to display on the series. By default, the organism with the highest number of observations is selected. If none is selected, **all** will be aggregated.|
| **Antimicrobial**       | Displays results for the selected antimicrobial. By default, the antimicrobial with the highest number of observations is selected. If none is selected, **all** will be presented as individual series.|
| **Sample Source / Site**| Restrict to selected body sites.                             |
| **Species (Host)**      | Select one or more host species.                             |
| **Timeframe**           | Specify a date range (manual or quick-select).               |

Several additional filters are available to refine the data shown. While many are self-explanatory, the following filters warrant further clarification:

| Filter                    | Description                                                                                                                  |
|---------------------------|------------------------------------------------------------------------------------------------------------------------------|
| **Antimicrobial Class**   | Displays only antimicrobials belonging to the selected class(es). Useful for focusing on specific drug groups.              |
| **Resistant to**          | Filters the dataset to include only isolates resistant to a selected antimicrobial. Helpful for exploring cross-resistance patterns. |
| **WHO AWaRe Class**       | Limits the display to antimicrobials categorized as *Access*, *Watch*, or *Reserve* according to the [WHO AWaRe classification](https://www.who.int/publications/i/item/2021-aware-classification). |

> *Note:* Applying filters will reduce the sample size and may affect stability of estimates. In **% Organism Prevalence** mode, antimicrobial-related filters (Antimicrobial, Antimicrobial Class, Resistant to, WHO AWaRe Class) still apply—they restrict the analysis to isolates that were tested against those antimicrobials. To analyze prevalence across *all* cultured isolates regardless of testing, select **"All cultures"** as your data source and clear antimicrobial-related filters.

---

## 🎛 Plot Controls<a name="plot-controls"></a>
To help interpret trends in noisy datasets, the **'Controls'** menu provides multiple options for smoothing the time series plots. These can make underlying patterns more visible by reducing short-term fluctuations.

- **Rolling Mean**  
  Applies a **centered moving average** to the data. This smooths out noise by averaging each value with its neighbors within a defined "window."  
  - **Window**: The number of time points (e.g., months or years) to include on either side of each data point. A larger window produces a smoother line but may obscure short-term trends.

- **LOESS (Locally Estimated Scatterplot Smoothing)**  
  Fits a **nonlinear regression curve** to the data using locally weighted fitting. LOESS is ideal for capturing complex, non-linear trends.  
  - **Span**: Controls how much of the data is used in each local regression. A smaller span produces a more flexible curve (closely follows the data), while a larger span results in a smoother, more generalized line.

### Binning (Prevalence Mode Only)

When analyzing **% Organism Prevalence**, you can choose how to group data over time:

- **Month** — Each data point represents one calendar month
- **Quarter** — Each data point represents a 3-month quarter (Q1, Q2, Q3, Q4)
- **Year** — Each data point represents one calendar year

Unlike the sample-size-based binning used for susceptibility analysis, time-based binning ensures that all organisms share the same temporal boundaries, making prevalence comparisons meaningful. For example, when binning by month, "January 2023" includes *all* isolates from that month for *all* organisms, so the denominator (total isolates) is consistent across organisms.

> 💡 **Tip:** Use **Month** for detailed temporal resolution, **Quarter** for seasonal patterns, or **Year** for long-term trends with smaller datasets.

> ⚠️ **Note:** While smoothing can clarify trends, it may also mask important variation. Always compare the smoothed line to the raw data before drawing conclusions. In **% Organism Prevalence** mode with **No smoothing**, hover text includes both the numerator (isolates of the selected organism) and denominator (total isolates) for transparency. Smoothed plots show only the denominator, as the smoothed values represent weighted averages across multiple time bins.

---

## 🔹 Legend<a name="legend"></a>

A legend appears on the right-hand side of the time series plot **only when multiple lines are displayed** (e.g., when comparing more than one antimicrobial or group). If a single series is plotted, no legend will be shown, as it is not needed for interpretation.

---

## 📤 Output<a name="output"></a>

- **Save Report** &mdash; download the current plot as an HTML file. The report includes the displayed plot, and all active filters used to generate it. Rendering may take a moment. 


---

## 💬 Feedback<a name="feedback"></a>

We are continually making improvements to this app and this display. Please report issues or suggestions via [GitHub Issues](https://github.com/ksobkowich/AMRDataVisualizer/issues).
