#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon May  7 23:16:35 2018

@author: AK47
"""
#Create geoJSON for R leaflet app

import pandas as pd, numpy as np 
import json, re, subprocess

df = pd.read_csv("city-council.csv")[['COUNCIL_DISTRICT','the_geom']]
colours = ['#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd']
d1 = df['the_geom'][0]

regex = r"(-[0-9]{2}\.[0-9]+)\s([0-9]{2}\.[0-9]+)"
begex = r"\("
cegex = r"\)"

subst = "[\\1, \\2]"
bsubst = "["
csubst = "]"

result = re.sub(cegex, csubst, re.sub(begex, bsubst, re.sub(regex, subst, d1, 0, re.MULTILINE), 0, re.MULTILINE), 0, re.MULTILINE)
result = result.split(" ", maxsplit=1)

def process(data):
    result = re.sub(cegex, csubst, re.sub(begex, bsubst, re.sub(regex, subst,
                    data, 0, re.MULTILINE), 0, re.MULTILINE),
                     0, re.MULTILINE).split(" ", maxsplit=1)[1]
    
    return result

df = df.assign(**{'SHAPE': df.the_geom.apply(lambda x: x.split(" ", maxsplit=1)[0]), "SHAPE_DATA": df.the_geom.apply(lambda x: process(x))})
#df = df.assign(**{'SHAPE': df.the_geom.apply(lambda x: x.split(" ", maxsplit=1)[0]), "SHAPE_DATA": df.the_geom.apply(lambda x: process(x)), "FILL_COLOUR": colours})

def mkdict(_id, shape, coords, colour=None):
    # Creates geoJson style dict needed for leaflet 
    
    shape = shape.lower()
    s1, s2 = shape[0:5], shape[5:]
    shape = ''.join([s1.capitalize(), s2.capitalize()])
    dct = {"type": "Feature",
           "geometry": {"type": shape,
                        "coordinates": json.loads(coords)
                        },
           "properties": {"district_id": _id}
           }
    
    
    return dct

fcol = {"type": "FeatureCollection",
    "features": []}

for (idx, row) in df.iterrows():
    dct = mkdict(str(row.loc['COUNCIL_DISTRICT']), row.loc['SHAPE'], row.loc['SHAPE_DATA'])
#    dct = mkdict(str(row.loc['COUNCIL_DISTRICT']), row.loc['SHAPE'], row.loc['SHAPE_DATA'], row.loc["FILL_COLOUR"])
    fcol["features"].append(dct)
    

    
with open('austin-council-processed.geojson', 'w') as jsonfile:
    json.dump(fcol, jsonfile)
    
subprocess.run(["geojson-rewind", "austin-council-processed.geojson", ">",  "austin-council-wound.geojson"])
    
    
