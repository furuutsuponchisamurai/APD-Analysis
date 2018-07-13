# APD Incident Analysis

I wanted to look at incidents recorded by APD over city council districts- the goal is to be able to sort by different types of crimes.

The file created by the python script isn't wound correctly according to the [geojson spec](https://tools.ietf.org/html/rfc7946). To fix the winding, I used [geojson-rewind](https://github.com/mapbox/geojson-rewind) from mapbox as a command line utility.