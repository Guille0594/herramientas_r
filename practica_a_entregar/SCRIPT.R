# Creation of a data set for the posterior MMM to quantify the effects
# of promotions on the sales of certain product. Anonymous data of an 
# unknown product and its weekly sales during 2 years.

library(readr)
library(janitor)
library(dplyr)
library(tibble)
library(ggplot2)

# EXERCISE 1 --------------------------------------------------------------

# Create a data frame called df_mmmm and clean it with the janitor package,

df_mmm<-read.csv("~/Desktop/R/practica_a_entregar/mktmix.csv",
                 na=c("NA","Nan",""))

df_mmm<-janitor::clean_names(df_mmm)
head(df_mmm)


# EXERCISE 2 --------------------------------------------------------------

# What are the classes of base_price and discount?
# How many columns and rows are there ?

class(df_mmm$base_price)
class(df_mmm$discount)
nrow(df_mmm)
ncol(df_mmm)

# EXERCISE 3 --------------------------------------------------------------

#Turn newspaper_insert's class into Numeric.

class(df_mmm$newspaper_inserts)

glimpse(df_mmm)

df_mmm <- df_mmm %>%                     
  mutate(newspaper_inserts = replace_na(newspaper_inserts, 0),
         newspaper_inserts = if_else(newspaper_inserts == "Insert", 1, 0 ))  

table(df_mmm$newspaper_inserts)
class(df_mmm$newspaper_inserts)


# EXERCISE 4 --------------------------------------------------------------


# How many values are there in website_campaign (NA does not count)

values_in_dfmmm <- table(df_mmm$website_campaign)

df_mmm <- df_mmm %>%                     
  mutate(website_campaign = replace_na(website_campaign, 0))
table(df_mmm$website_campaign)

# Create new columns from each category defined as: 1 if website campaign 
# equals Facebook and 0 if it does not, same with all.


df_mmm <- df_mmm %>%    
  mutate(facebook=if_else(website_campaign=="Facebook", 1, 0),
         twitter=if_else(website_campaign=="Twitter",1,0),
         website_campaign=if_else(website_campaign=="Website Campaign",1,0))

glimpse(df_mmm)
df_mmm
table(df_mmm$website_campaign)


# EXERCISE 5 --------------------------------------------------------------

# Create a line plot with ggplot2 showing the evolution of new_vol_sales, which
# would be the target variable in a model. Since we haven’t been provided with 
# dates, you will have to invent an x axis (it can be just numbers from 1 and
# so on).


new_vol_sales_evolution <- df_mmm %>% 
  ggplot()+
  theme_light() +
  geom_line(aes(x=c(1:104),y=new_vol_sales), colour="red", alpha=0.6,
            size=1, lty="solid")+
  labs(x="Days", y="Sales Volume")

new_vol_sales_evolution
  

# EXERCISE 6 --------------------------------------------------------------

# Create a histogram and a boxplot of the same variable. Based on the plots, 
# which is the median of the variable? Calculate it with an R function. Were 
# you close?

# Histogram   
  
df_mmm %>% 
  ggplot() +
  geom_histogram(aes(x=new_vol_sales), color="black", fill = I("aquamarine2"),
                 binwidth = 300) +
  labs(x="Count", y="Sales Volume", title = "New Vol Sales") + 
  geom_vline(aes(xintercept = median(new_vol_sales)),col='red',size=1)

# ßoxplot
  
df_mmm %>% 
  ggplot() +
  geom_boxplot(aes(y=new_vol_sales), fill="lightgreen") +
  labs(x="Count", y="Sales Volume", title = "New Vol Sales") +
  geom_hline(aes(yintercept = median(new_vol_sales)), col ="red", size = 1,
             linetype=2,)
  

# La mediana parece estar en torno a 20,000, pasamos a verlo con la función
# de R base:

mean_new_vol_sales <- mean(df_mmm$new_vol_sales)
mean_new_vol_sales


# EXERCISE 7 --------------------------------------------------------------

# Select only the media investment columns: tv, radio and stout, and create a 
# new data frame just with them. Use this data frame and the provided code for 
# creating a plot with the evolution of these three columns. This should be a 
# plot in an only figure, with a share x axis but different y axis (see the 
# result). For using the provided code, suppose the data frame you created with
# just the media data is called df_media. Is there anything worth mentioning 
# from the plot?

df_media <- df_mmm %>% 
  select(tv, radio, stout) 


df_media <- df_media %>%
  pivot_longer(everything()) 

vector_rank <- rep(1:104, each = 3)
df_media <- cbind(df_media, vector_rank)

 
ggplot(df_media, aes(x = vector_rank, y = value, col = name)) +
  geom_line() +
  facet_wrap(~name, nrow=3, scales = "free") + 
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())

# Es destacable que hay dos grupos de días en los que no existen inversiones en 
# radio, a parte de los últimos días del período, en los que los valores de 
# radio son desconocidos. 


# EXERCISE 8 --------------------------------------------------------------

# in_store is an index of the stock available on stores for selling the product.
# Create a scatter plot with ggplot2 for comparing the new_vol_sales column 
# against in_store. Choose carefully which column should be set on the x axis 
# and which on the y, considering that new_vol_sales will be the target variable
# on a model, i.e., the analyst will want to explain this variable based on the 
# rest of the information. Explain your decision and also comment anything 
# interesting from the plot. For doing this, think about the relation that could
# exist between the stock of a product and its sales.

df_mmm %>% 
  ggplot(aes(x=in_store, y=new_vol_sales)) +
  geom_point(shape=15,color="darkblue", size=1.5)+
  geom_smooth(method = "lm", 
              se = FALSE, 
              aes(x=in_store, y=new_vol_sales), colour="red") + 
  labs(title="Relationship betweeen stock in store and sales")

# New_vol_sales es la varibale será la que queramos predecir, por eso tiene que
# ir en el eje y, como yo entiendo en el eje x la causa y el eje y el efecto.

# En el gráfico, dibujando la línea, se puede ver que si existe relación en 
# cuanto a > stock en tienda, mayores sol las ventas. No obstante, hay otras
# cosas a tener en cuenta como el precio de mantener productos en el almacén. 
# Si decidimos colorear los puntos en función de los anuncios en peródico como 
# hacemos en el siguiente ejercicio, se ve que no tiene un gran efecto en las 
# ventas el hecho de anunciarse en el periódico.

# EXERCISE 9 --------------------------------------------------------------

# Create two different versions of the previous plot:

#  -Color each dot differently based on the newspaper_inserts column 
# (using as.factor() here is recommended).

# - Color each dot differently based on the tv column.

df_mmm %>% 
  ggplot(aes(x=in_store, y=new_vol_sales)) +
  geom_point(shape=1, size=1.5, aes(col=as.factor(newspaper_inserts))) +
  labs(title="Relationship betweeen stock in store and sales") + 
  geom_hline(aes(yintercept = median(new_vol_sales)),col='red',size=0.2)

df_mmm %>% 
  ggplot(aes(x=in_store, y=new_vol_sales)) +
  geom_point(size=2.5, aes(col=tv)) +
  labs(title="Relationship betweeen stock in store and sales")

# es hline porque es horizontal y sale del y. 


# EXERCISE 10 -------------------------------------------------------------

# Create another column on the data frame indicating whether a discount has 
# been applied or not. You can name discount_yesno, for example. The column
# can be numerical or logical.

# After that, create another data frame aggregating
# the original one, for calculating the average base price when there’s a 
# discount and where it isn’t. 

# Use this data frame for creating a column plot with ggplot. On the x axis
# you should use the discount_yesno values and on the y axis, the average 
# price you calculated.

# Are the any significant differences? Remark. Try not to create new data 
# frames nor overwriting the original one, but chain all the operations
# with %>%, including the plot.

df_mmm %>% 
  mutate(discount_yesno = if_else(discount != 0, TRUE, FALSE)) %>% 
  group_by(discount_yesno) %>% 
  summarise(base_price_average=mean(base_price)) %>% 
  ggplot(aes(x=discount_yesno, y=base_price_average, fill=discount_yesno))+
  geom_bar(stat = "identity", position = "dodge")+
  xlab("Discount applied")+
  theme_bw()+
  ylab("Base average price")

# En los gráficos puede verse que no existe una gran diferencia en 
# la media del base_price haya o no descuento. Otro estudio interesante
# sería comprobar si con el descuento aplicado el volumen de ventas es 
# mayor o menor.

  
# EXERCISE 11 -------------------------------------------------------------

# Create a function that fits a model on this dataset using the provided code.
# The idea of the function is selecting a subset of columns on the data frame,
# creating a data frame with this selection, using the data frame on a model 
# and returning a number that will indicate how good the model is. So the input
# will be a character, that will be the names of the columns, and the output 
# will be this fitness number, provided in the next piece of code. 
# You have been asked to create an auxiliary data frame with the radio stout tv
# value selection of columns: for this piece of code we are assuming you’ve
# called the data frame df_aux but you can name it as you want and change the code.



my_select <- function (cols) {
    if (!"new_vol_sales" %in% cols) {
    cols= c(cols, "new_vol_sales")
    print(cols) }
    if(!all(cols %in% colnames(df_mmm))){
      stop("Columnas no presentes en el data frame")
    }
  df_aux  <- df_mmm[,cols]                               
  my_model<- lm(new_vol_sales ~ ., data = df_aux)    
  summary(my_model)$adj.r.squared
  
}

cols <- c("in_store", "base_price","new_vol_sales")
my_select(cols)

# EXERCISE 12 -------------------------------------------------------------

# You are given three sets of variables. Create a list whose elements will be 
# these three vector. Now, using map_dbl() or sapply(), call the function you 
# created in the previous exercise for the three cases. Which of the three 
# subsets provide the best model, bearing in mind that the larger the returned
# number, the better?

c1 <- c("base_price", "radio", "tv", "stout")
c2 <- c("base_price", "in_store", "discount", "radio", "tv", "stout") 
c3 <- c("in_store", "discount")

lista_pruebas <- list(c1,c2,c3)

sapply(lista_pruebas, my_select)

# El mejor modelo lo explica c2, lo cual tiene sentido ya que estamos intentando
# explicar la variable independiente volumen de ventas en función de otras 
# independientes, y en c2 están incluidas todas las de los otros dos vectores,
# por lo tanto tiene que ser superior.


  
  
  
  
  
  
  



  
 
      

         
      


    

