# Data dictionary

| Variable | Type | Role |
|---|---|---|
| `Date` | Date | First day of the monthly period |
| `Year` | Integer | Calendar year derived from Month |
| `MonthNumber` | Integer | Calendar month from 1 to 12 |
| `MonthName` | Ordered factor | Readable month label |
| `Country` / `CountryShort` | Categorical | Country of residence among eight represented markets |
| `TravelPurpose` | Categorical | Business, Holiday/Vacation, or Visit Friends/Relatives |
| `Age` / `AgeFactor` | Ordered categorical | Sixteen published age groups |
| `Sex` | Categorical | Published sex category |
| `VisitorArrivals` | Numeric response | Estimated passenger movements for the cell |
| `TimeIndex` | Numeric feature | Sequential month index used by time-aware models |
| `Recovered` | Binary response | Whether a country-purpose month reached its matching 2019 level |

The exact derived columns differ by script. Read the numbered R files for each
model's feature construction.
