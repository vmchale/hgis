### About

This is a library and command-line tool for GIS. 

Currently, it generates maps and computes perimters, areas, and compactness. You can use it to look at your congressional district for evidence of gerrymandering. 

### Installation

hgis is best installed with [stack](https://haskellstack.org/). Try

```
stack install hgis
```

or

```
stack install hgis --flag hgis:-cairo
```

to install with bindings [cairo](https://cairographics.org/) library, which will enable output as PNGs. 

You can also clone this repository and type `stack build` if you want to start hacking. 

### Usage

#### Command-line

From the command line, type

```
hgis FILE.shp OUTPUT.svg
```

to read a shapefile and write an svg. 

#### Library

### Documentation

#### Haddock
