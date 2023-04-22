### Phillip & Danish

setwd("C:/Users/isma-/OneDrive/Escritorio/full databse/DATA")
df <- read_excel("Global_dataset.xlsx")
df1<- df %>% unite(Link, c("site_id", "taxon"))

a<- df %>% filter(Alien=="Y") %>% group_by(site_id,taxon) %>%  
  summarise(Count_alien =n()) %>% filter(Count_alien > 4) %>% group_by(taxon) %>%
  summarise(Number_of_time_series =n()) %>% arrange(-Number_of_time_series)
a
write.csv2(a,"a.csv")


a<- df %>% filter(Alien=="Y") %>% group_by(site_id,taxon) %>%  
  summarise(Count_alien =n()) %>% filter(Count_alien > 4) %>% unite(Link, c("site_id", "taxon"))

c<- semi_join(df1, a, by = "Link")
c <- c[-c(4,5,6)]
write_xlsx(c, "Danish.xlsx")


# Min year of time series

df <- read_excel("Global_dataset.xlsx")
df1<- df %>% unite(Link, c("site_id", "taxon"))

a<- df %>% filter(Alien=="Y") %>% group_by(site_id,taxon) %>%  
  summarise(Count_alien =n()) %>% filter(Count_alien > 4) 


c<- semi_join(df, a, by = "site_id")
c<- df %>% filter(Alien=="Y")
unique(df1$taxon)
d<- c %>% group_by(taxon) %>% summarise(min=min(year),
                                    max=max(year))

e<- c %>% group_by(taxon) %>% summarise(count=n())


###### Calculate relative abundancee

df <- read_excel("Global_dataset.xlsx")

df1<- df %>% group_by(site_id,year) %>% summarise(Total_abundance=sum(abundance))
df1<- df1 %>% unite(Link, c("site_id", "year"))

df <- read_excel("Danish.xlsx", sheet = "Taxa selected")

write_xlsx(df1, "Total_abundance.xlsx")


### Table Stwist vs first record

a<- df %>% filter(Alien=="Y") %>% group_by(site_id, taxon) %>%  
  summarise(Count_alien =n()) %>% filter(Count_alien > 4)

a

c<- semi_join(df,a, by="site_id") %>% filter(Alien=="Y") %>% group_by(taxon, country) %>%
  summarise(Min=min(year))
c
write_xlsx(c, "First_record.xlsx")


setwd("C:/Users/isma-/OneDrive/Escritorio/Phillip+Danish")

df<- read_excel("First_record.xlsx", sheet = "Sheet2")
head(df)

ggplot(df,aes(x=sTWIST, y=as.factor(taxon), group=as.factor(country),
              colour=as.factor(country))) + geom_point(position=position_dodge(width = 1.5))  



#Number of time series over time
setwd("C:/Users/isma-/OneDrive/Escritorio/full databse/DATA")
df <- read_excel("Global_dataset.xlsx")


a<- df %>% filter(Alien=="Y") %>% group_by(site_id,taxon) %>%  
  summarise(Count_alien =n()) %>% filter(Count_alien > 4)
head(a)

c<- semi_join(df, a, by = "site_id") %>% group_by(site_id,year) %>% summarise(count=n()) %>%
  mutate(Suma=1) %>% group_by(year) %>% summarise(Time_series=sum(Suma))

ggplot(c, aes(year,Time_series)) + geom_point() + geom_line() + theme_bw() + 
  scale_y_continuous(limits = c(0, 565), breaks = seq(0, 565, by = 100))




##### PLot suggested by Phillip (Core points on invasion curve)
setwd("C:/Users/isma-/OneDrive/Escritorio/Phillip+Danish")
df <- read_excel("Plot_core_points.xlsx", sheet = "Sheet2")
head(df)
str(df)


ggplot(df, aes(as.factor(`Core point`),Year, ymin=Year-SE, ymax=Year+SE))  +
theme_bw()+
  geom_pointrange(aes(color= as.factor(Group), shape =as.factor(Group)), 
                  size=0.8,position = position_dodge(0.5)) +
  scale_y_continuous(breaks = seq(from = -50, to = 80,by = 20))
  
  
  



a<- a %>% unite(Link, c("site_id", "taxon"))
a<- a %>% separate(Link, c("site_id", "taxon"), "_")

d<- d <- df %>% group_by(site_id) %>% summarise(Min=min(year),
                                     Max=max(year),
                                     Average_year= Max-Min)

c<- semi_join(df1,a, by="Link")
c<- c %>% separate(Link, c("site_id", "taxon"),"_")
c<- c %>% group_by(site_id,taxon) %>% summarise(min=min(year),
                                           max=max(year),
                                           Total= max-min)


write.csv2(a,"aaaa.csv")
