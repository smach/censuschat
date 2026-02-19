# tool_get_place_tracts code generation snapshot

    Code
      cat(result)
    Output
      {
        "message": "To get tract data for Framingham , run this code in R.",
        "explanation": "Tracts don't nest directly under places (cities/towns), so we need to use spatial filtering with tigris boundaries.",
        "tidycensus_code": "library(tidycensus)\nlibrary(tigris)\nlibrary(sf)\nlibrary(dplyr)\n\n# Get tract data with geometries\ntracts <- get_acs(\n  geography = \"tract\",\n  variables = c(\"B19013_001\", \"B25077_001\"),\n  state = \"MA\",\n  survey = \"acs5\",\n  year = 2022,\n  geometry = TRUE\n)\n\n# Get place boundary and filter to tracts that intersect\nplace <- places(state = \"MA\", year = 2022) |>\n  filter(grepl(\"Framingham\", NAME, ignore.case = TRUE))\n\ntracts_in_place <- tracts |>\n  st_filter(place, .predicate = st_intersects)\n\nprint(tracts_in_place)",
        "packages_needed": ["tidycensus", "tigris", "sf", "dplyr"],
        "install_command": "install.packages(c(\"tidycensus\", \"tigris\", \"sf\", \"dplyr\"))"
      }

