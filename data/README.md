# Data setup

## Source

The working data came from a transformed extract of Stats NZ Infoshare
International Travel data under Tourism, International Travel and Migration.

- Source portal: <https://infoshare.stats.govt.nz/Default.aspx>
- Metadata: <https://datainfoplus.stats.govt.nz/Item/nz.govt.stats/f705ca38-ea6e-453f-b1d9-a95dc0fcaf59>

## Required local file

Place the transformed workbook at:

```text
data/raw/visitor_arrivals_transformed.xlsx
```

The workbook must contain a sheet named `flattened dataset` with these columns:

| Column | Meaning |
|---|---|
| Month | Monthly period |
| Country of Residence | One of the eight represented markets |
| Travel Purpose | Business, Holiday/Vacation, or Visit Friends/Relatives |
| Age | One of sixteen age groups |
| Sex | Published sex category |
| Visitor Arrivals | Estimated passenger movements for the cell |

The raw workbook is intentionally excluded from GitHub. The R scripts create
analysis-ready files under `data/processed/`.

## Unit of observation

One row represents a Month × Country × Travel Purpose × Age × Sex cell. It is
not a record for one traveller and does not count unique people. A traveller
who crosses the border more than once may contribute multiple passenger
movements.
