qca.sim
=======

***qca.sim*** is a suite of functions written in R that can be used to search for QCA solutions across all possible combinations of calibration and logical reduction parameters, as well as to assess the sensitivity of QCA results.

Its constituent functions include:
- ***QCA.sim***: a function written in R that returns QCA solutions for a range of minimum frequency thresholds and an arbitrarily large number of sufficiency inclusion score pairs.
- ***QCA.sim.inclcut***: a function written in R that returns QCA solutions for a given minimum frequency threshold and an arbitrarily large number of sufficiency inclusion score pairs.
- ***QCA.random***: a function written in R that returns QCA solutions for a given minimum frequency threshold and an arbitrarily large number of sufficiency inclusion score pairs when a random variable is repeatedly added to the dataset.
- ***get.qca.datasets***: a function written in R that automatically downloads raw and calibrated QCA datasets hosted on the COMPASSS.org server. Can select between csQCA, mvQCA, and fsQCA. Returns datasets as a list, along with hyperlink selections indicating the author name, year, and dataset type (raw versus calibrated).
