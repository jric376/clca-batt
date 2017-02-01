# Estimating profits and emissions of solar + storage

This repo contains code for simulating the use of renewable and distributed energy resources to reduce peak loads in buildings. It allows for subsequent calculation and visualization of these systems' financial performance and net emissions impact, using existing utility rates and power plant data to do so. For now, it assumes that the solar + storage only carry out peak shaving services, though other operational strategies are possible, and that New York City is the location for the building and various energy resources.

The repo contains objects that represent building energy consumption models, solar and energy storage systems, and the time-varying subsets of power plants that provide electricity. The latter involves generating a curve that arranges power plants in increasing order by marginal cost of operation, and then calculating the cumulative emissions impact of the plants as you traverse the curve. From there, the simulations traverse the time-series of building and grid loads, plus solar generation, and (dis)charge energy storage accordingly. Along the way, estimates of the emissions impacts of the solar + storage are accumulated.

The eventual goal is to have the scripts allow for flexible simulation of different building types, different grid services, and different locations.

Please drop a line if you are interested in getting sample data too large for storage on GitHub, which you will otherwise have to create manually.