# FrenchNFIfindeR
Get and format French National Forest Inventory data

## Getting started

To use this package, you first need to install the latest version from this repository

```
remotes::install_github("Jeremy-borderieux/FrenchNFIfindeR")
```
The main function of this package is `get_NFI`, that tries to download and load in the environement the latest version of the NFI. This function returns data.tables, make sure to use the `data.table` package or to convert them before usage.
You can get more informations about the datasets themselves and download options with `?get_NFI`

```
get_NFI()
```
## Source
The data and their documentation are the intellectual property of the IGN (French Nationl Institue of Geography). They can be reused free of charge according to the Open Licence Etalab Version 2.0. 
If you are using the NFI, cite the data with :
"IGN – Inventaire forestier national français, Données brutes, Campagnes annuelles 2005 et suivantes, https://inventaire-forestier.ign.fr/dataIFN/, site consulté le XX/XX/20XX"
