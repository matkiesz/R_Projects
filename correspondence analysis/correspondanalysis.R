# Contingency table

install.packages(c("FactorMineR", "factoextra", "gplots"))
install.packages("FactoMineR")
library("FactoMineR")
library("factoextra")
library("gplots")
library("titanic")
library("RPostgreSQL")

data(housetasks)
head(housetasks)

dt <- as.table(as.matrix(housetasks))
balloonplot(t(dt), main = "housetasks", xlab = "", ylab = "", label = TRUE, show.margins = FALSE)

ca_result <-CA(housetasks, graph = FALSE)
ca_result

eig.val <- get_eigenvalue(ca_result)
eig.val

# druga metoda
fviz_screeplot(ca_result,addlabels = TRUE, ylim = c(0,50))

fviz_contrib(ca_result,choice = "row", axes = 1, top = 10)
fviz_contrib(ca_result,choice = "row", axes = 2, top = 10)

fviz_ca_row(ca_result)
fviz_ca_col(ca_result)

fviz_ca_biplot(ca_result)
fviz_ca_biplot(ca_result, col.row = "contrib")

# titanic

catable <- titanic_train[c("Pclass","Fare")]
catable

dt <- table(catable)

balloonplot(t(dt), main = "titanic", xlab = "", ylab = "", label = TRUE, show.margins = FALSE)
titanic_result <-CA(dt, graph = FALSE)

titanic_result
eig.val <- get_eigenvalue(titanic_result)
eig.val

fviz_ca_biplot(titanic_result)

# 2.1


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "postgres",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "xxx")
df_postgres <- dbGetQuery(con, "SELECT typ_wniosku,partner FROM wnioski
WHERE partner is NOT NULL")
df_postgres
df_postgres <- table(df_postgres)
class(df_postgres)

balloonplot(t(df_postgres), main = "claims", xlab = "", ylab = "", label = TRUE, show.margins = FALSE)


claims_result <-CA(df_postgres, graph = FALSE)

fviz_screeplot(claims_result,addlabels = TRUE, ylim = c(0,100))
fviz_contrib(claims_result,choice = "row", axes = 1, top = 10)
fviz_contrib(claims_result,choice = "row", axes = 2, top = 10)

claims_result
eig.val2 <- get_eigenvalue(claims_result)
eig.val2

fviz_ca_biplot(claims_result,col.row = "contrib")

#2.2

df_postgres2 <- dbGetQuery(con, "SELECT wylot_kod_regionu, przylot_kod_regionu FROM o_trasy")

df_postgres2 <- table(df_postgres2)

balloonplot(t(df_postgres2), main = "claims", xlab = "", ylab = "", label = TRUE, show.margins = FALSE)
region_result <- CA(df_postgres2, graph = FALSE)
fviz_screeplot(region_result,addlabels = TRUE, ylim = c(0,70))
fviz_contrib(region_result,choice = "row", axes = 1, top = 10)
fviz_contrib(region_result,choice = "row", axes = 2, top = 10)

eig.val3 <- get_eigenvalue(region_result)
fviz_ca_biplot(region_result,col.row ="contrib")



