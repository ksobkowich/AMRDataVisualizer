<!-- File: antibiograms.md -->
# 📊 Antibiogram Tab

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

The **Antibiogram** tab delivers a concise, clinically relevant view of antimicrobial resistance (AMR) patterns. An antibiogram is a cumulative report that summarizes the **percentage of bacterial isolates susceptible (S)** to each antimicrobial.

---

## 📖 How to Interpret the Figure<a name="how-to-interpret-the-figure"></a>

Each cell shows the **percentage of isolates interpreted as Susceptible (S)** for a given organism–drug pair. That percentage is calculated over **all interpretation categories** (S, I, R, N/I, SDD), so its inverse is **not** the same as the percent Resistant (R).

| Key Element              | What to Look For                                                                                                      |
|--------------------------|------------------------------------------------------------------------------------------------------------------------|
| **Susceptibility %**     | High values = greater antimicrobial activity against that organism.                                                    |
| **Sample Size (n)**      | Estimates based on **\< 30 isolates** are less reliable. Cells with low counts are uncolored by default but can be shown via *Plot Controls*. Hover any value to see its exact *n*. |
| **Row / Column Layout**  | Rows list microorganisms (or other selected variable); columns list antimicrobials.                           |
| **Local Context**        | Resistance patterns are highly regional—interpret results within the context of your facility or dataset origin.      |

---

## ⚠️ Points of Caution<a name="points-of-caution"></a>

- **Limited Scope** &mdash; Aggregated historical data; does **not** replace patient-specific testing.  
- **Evolving Resistance** &mdash; Effective drugs today may lose efficacy over time.  
- **Clinical Judgment Required** &mdash; AST data are often biased towards more resistant infections, and the degre of this bias depends on several factors.

---

## 🧰 Data Filters<a name="data-filters"></a>

Customize the antibiogram using the **Filters** panel (click the *pencil* icon ✏️ in the top right corner of the panel to add/remove fields).

| Default Filter          | Purpose                                                      |
|-------------------------|--------------------------------------------------------------|
| **Microorganism**       | Limit plot to one or more organisms. If none is selected, **all** will be included.                         |
| **Sample Source / Site**| Restrict to selected body sites. If none is selected, **all** will be included.                             |
| **Species (Host)**      | Select one or more host species.                             |
| **Timeframe**           | Specify a date range (manual or quick-select).               |
| **Suppress Antimicrobials** | Exclude selected drugs (e.g., Reserve agents) to emphasize preferred first-line options. |

Several additional filters are available to refine the data shown. While many are self-explanatory, the following filters warrant further clarification:

| Filter                     | Use Case                                                                                                                    |
|----------------------------|-----------------------------------------------------------------------------------------------------------------------------|
| **Antimicrobial Class**    | Show only selected drug classes.                                                                                            |
| **Resistant to:**  | Display isolates resistant to a chosen drug, useful for cross-resistance analysis.                                          |
| **WHO AWaRe Class**        | Limit drugs to *Access*, *Watch*, or *Reserve* categories per [WHO AWaRe](https://www.who.int/publications/i/item/2021-aware-classification). |

> *Note:* Applying filters will reduce the sample size and may affect stability of estimated susceptiblity.

---

## 🎛 Plot Controls<a name="plot-controls"></a>

| Control                     | Description                                                                                                                |
|-----------------------------|----------------------------------------------------------------------------------------------------------------------------|
| **Y-Axis Variable**         | Choose the grouping variable (e.g., *Microorganism*, *Body Site*).                                                         |
| **Sort By**                 | Order Y-axis groups by name, susceptibility, or custom metric.                                                             |
| **Antibiogram Style**       | *Classic* (table of values) or *Visual* (bubble plot).                                                                     |
| **Handle Low Counts**       | Keep or exclude rows/columns with **\< 30 isolates** per [CLSI M39](https://clsi.org/shop/standards/m39/).                 |
| **Maximum Rows**            | Limit the number of Y-axis categories displayed.                                                                           |
| **Show Colors**             | Toggle color shading in cells.                                                                                             |
| **Aggregate by Genus**      | When Y-axis = *Microorganism*, group species into genera.                                                                  |
| **Split by Gram Stain**     | When Y-axis = *Microorganism*, create separate plots for Gram-positive and Gram-negative organisms.                        |

---

## 🔹 Legend<a name="legend"></a>

| Susceptibility % | Classic Antibiogram&nbsp;<br>(cell color)                  | Visual Antibiogram&nbsp;<br>(circle size) |
|------------------|------------------------------------------------------------|-------------------------------------------|
| ≥ 90&nbsp;%      | <span style="display:inline-block;width:16px;height:16px;background:#44CDC4;border:1px solid #ccc;"></span> | Large |
| 70–89&nbsp;%     | <span style="display:inline-block;width:16px;height:16px;background:#FEE08B;border:1px solid #ccc;"></span> | Medium |
| \< 70&nbsp;%     | <span style="display:inline-block;width:16px;height:16px;background:#D73027;border:1px solid #ccc;"></span> | Small |

*Classic Mode* colors can be toggled in **Plot Controls**.  
In *Visual Mode*, circle **color** denotes antimicrobial class, and
**opacity** fades for sample sizes \< 30 to flag low reliability
([CLSI M39](https://clsi.org/shop/standards/m39/)).

---

## 📤 Output<a name="output"></a>

- **Save Report** &mdash; download the current plot as an HTML file. The report includes the displayed plot, and all active filters used to generate it. Rendering may take a moment. 
- **Save Data** &mdash; export the summary table with exact S percentages and sample sizes.

---

## 💬 Feedback<a name="feedback"></a>

We are continually making improvements to this app and this display. Please report issues or suggestions via [GitHub Issues](https://github.com/AMR-Visualizer/AMRDataVisualizer/issues).
