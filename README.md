# State-Daylight-Visualizer-RShiny-app
An R Shiny App that generates visualizations for annual sunlight patterns across any U.S. state. It calculates and plots key solar events, including sunrise/sunset times, daylight hours, sun angle, twilight duration, and solstices. The dashboard is fully dynamic and cached for speed.

# R Sun Almanac: A State Daylight Visualizer

> A dynamic R Shiny dashboard for visualizing annual sunlight patterns across any U.S. state. Select a state to generate a full suite of plots for key solar events like sunrise/sunset times, daylight hours, sun angle, twilight duration, and solstices.

<img width="1968" height="2586" alt="image" src="https://github.com/user-attachments/assets/ddde3665-3b51-4974-bcfd-a1c45da2e7f4" />

<img width="2012" height="1394" alt="image" src="https://github.com/user-attachments/assets/481e8498-febc-4620-b938-fb32c6c7d60c" />


---

## About This Project

This application provides a comprehensive, interactive almanac of solar data for any state in the United States for the year 2025. What began as a static R script for a single state has evolved into a fully interactive Shiny web application. The goal is to make detailed solar event data accessible and easy to visualize.

By simply choosing a state and clicking "Generate Plots," the app produces ten distinct visualizations that explore different facets of the solar year.

## Features

The dashboard generates the following ten plots:

1.  **Monthly Daylight Change:** A grid of contour maps showing how the amount of daylight changes from the beginning to the end of each month across the state's geography.
2.  **Annual Daylight Curve:** A line plot comparing the daylight hours in the northern, central, and southern parts of the state throughout the year.
3.  **Average Monthly Change:** A bar chart showing the average rate of change in daylight, highlighting the rapid changes around the equinoxes.
4.  **Daylight Symmetry:** A plot demonstrating the near-perfect symmetry of daylight hours between the first and second halves of the year around the summer solstice.
5.  **Sunrise & Sunset Times:** A graph of the daily sunrise and sunset times, clearly showing the abrupt shifts caused by Daylight Saving Time.
6.  **Solar Noon Variation:** A plot of the "equation of time," showing how solar noon (when the sun is highest in the sky) deviates from clock noon.
7.  **Twilight Duration:** A comparison of the total daily civil twilight duration across different latitudes within the state.
8.  **Extreme Events Calendar:** A timeline marking the exact dates of the year's longest/shortest days and earliest/latest sunrises/sunsets.
9.  **Sun Angle Comparison:** A plot comparing the sun's maximum angle (altitude) at solar noon for the north, center, and south of the state.
10. **Seasonal Sun Angle:** A detailed view of the solar noon sun angle for the state's center, with key dates (solstices and equinoxes) highlighted.

## Technical Highlights

-   **Dynamic & Interactive:** The entire analysis is re-run dynamically based on user input, with all plot titles, legends, and data updating automatically.
-   **Parallel Processing:** The most computationally intensive task—calculating data for a 100x100 grid of geographic points—is parallelized using the `{furrr}` package to dramatically speed up initial calculations.
-   **Persistent Caching:** After data for a state is calculated once, it is saved to an `.rds` file in the `cache/` directory. Subsequent requests for the same state will load from the cache almost instantly, bypassing the need for re-computation.
-   **Spatial Analysis:** Uses the `{sf}` package to accurately filter a grid of coordinates to only those that fall within a state's political boundaries.
-   **Dynamic Timezones:** Automatically detects the correct local timezone for any state using `{lutz}`, ensuring that phenomena like Daylight Saving Time are handled correctly.

## How to Run Locally

To run this application on your own machine, follow these steps.

### Prerequisites

-   R (version 4.0 or newer recommended)
-   RStudio (recommended for the best experience)

### Installation & Setup

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/Cuevman81/State-Daylight-Visualizer-RShiny-app.git
    cd State-Daylight-Visualizer-RShiny-app
    ```

2.  **Install required packages:**
    Open R or RStudio and run the following command in the console to install all necessary dependencies.
    ```R
    install.packages(c("shiny", "shinycssloaders", "suncalc", "ggplot2", "dplyr", "lubridate", "sf", "maps", "metR", "tidyr", "purrr", "ggrepel", "lutz", "tools", "furrr"))
    ```

3.  **Create the cache directory:**
    In the root of the project folder, create a new, empty directory named `cache`. This is required for the caching system to work.

    ```bash
    # From your terminal in the project directory
    mkdir cache
    ```

4.  **Run the app:**
    Open the `app.R` file in RStudio. The "Run App" button will appear at the top of the script editor pane. Click it to launch the application.

## Credits & Acknowledgements

-   **Author:** Rodney Cuevas
-   **Core Package:** This project relies heavily on the brilliant `{suncalc}` package for all solar position and timing calculations.
-   **Visualization:** All plots are created with `{ggplot2}` and its ecosystem.
