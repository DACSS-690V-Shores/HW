
# get data ------------------------------------------------------------

rm(list = ls()) # clean memory

location='https://github.com/DACSS-Visual/tabular_univar_cat/raw/main/data/'
file='eduwa.rda'
link=paste0(location,file)

#getting the data TABLE from the file in the cloud:
load(file=url(link))


# see data ----------------------------------------------------------

dim(eduwa)
names(eduwa)
head(eduwa)


# see data types ----------------------------------------------------------

str(eduwa)