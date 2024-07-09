



#   Foil Tunnel Leaf     #
# nutrient analysis 2023 #

#Document set up ================
#BM
# we have all the data except N concentration from June, and some scans from:
# Super important that the same dates are used for each sampling event: the dates used here are obtained from the spectroradiometric data and are: scans 103 to 255 from 2 May
# 2023-05-02
# 2023-05-30
# 2023_06_09
# 2023-06-30
# 2023-07-10

# they must be changed in the names of the file and the dates in the files themselves

#general libraries
set.seed(4444)
library(tidyverse)

#set working directory: all paths are relative to the script location
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
#load("Foil_tunnel_leaf_nutrients_2023_workspace.RData")

#function for extracting variables stored in lists
get_elements <- function(x, element) {
  if (is.list(x))
  {
    if (element %in% names(x))
      x[[element]]
    else
      lapply(x, get_elements, element = element)
  }
}

library(ggsci)
scale_fill <- scale_fill_jco
scale_colour <- scale_colour_jco
#import data ==========================
#create a file list of names
library(fs)
nutrient.content.files.P <-
  dir_ls("../Analysis Results/Phosphorus") %>% as.list()
nutrient.content.files.N <-
  dir_ls("../Analysis Results/Nitrogen/") %>% as.list()

##Foliar Nutrient Content data (lab) ========================
### Phosphorus ==============================================
library(readxl)
nutrient.content.P.by.date <-
  lapply(nutrient.content.files.P, function(file) {
    read_excel(
      file,
      col_types = c(
        "skip",
        "text",
        "text",
        "text",
        "skip",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "text",
        "numeric",
        "text",
        "numeric",
        "numeric",
        "numeric",
        "numeric"
      ),
      skip = 1
    )
  })
names(nutrient.content.P.by.date) <-
  list.files("../Analysis Results/Phosphorus/") %>%
  str_replace(".xlsx", "")

#combine into one data frame and rename

# [1] "Phosphor (P) (%)"      "Kalium (K) (%)"
# [3] "Calcium (Ca) (%)"      "Magnesium (Mg) (%)"
# [5] "Bor (B) (mg/kg)"       "Eisen (Fe) (mg/kg)"
# [7] "Mangan (Mn) (mg/kg)"   "Kupfer (Cu) (mg/kg)"
# [9] "Zink (Zn) (mg/kg)"     "Schwefel (S) (%)"
# [11] "Natrium (Na) (mg/kg)" "Silizium (Si) (mg/kg)"

nutrient.content.P.all.dates <-
  bind_rows(nutrient.content.P.by.date, .id = "Date") %>%
  `colnames<-`(
    c(
      "Index",
      "Date",
      "Treatment",
      "Cultivar",
      "P",
      "K",
      "Ca",
      "Mg",
      "B",
      "Fe",
      "Mn",
      "Cu",
      "Zn",
      "S",
      "Na",
      "Si"
    )
  )

#check variable classes
library(lubridate)
sapply(nutrient.content.P.all.dates, class)
nutrient.content.P.all.dates$Date <-
  dmy(nutrient.content.P.all.dates$Date)

#all values below the limit of detection are set to the limit
nutrient.content.P.all.dates <-
  nutrient.content.P.all.dates %>%
  mutate(Fe = as.numeric(str_replace(Fe, "< ", ""))) %>%
  mutate(Cu = as.numeric(str_replace(Cu, "< ", "")))


###Nitrogen ===================================================

nutrient.content.N.by.date <-
  lapply(nutrient.content.files.N, function(file) {
    read_excel(
      file,
      col_types = c("skip", "text", "text",
                    "text", "skip", "numeric"),
      skip = 1
    )
  })

names(nutrient.content.N.by.date) <-
  list.files("../Analysis Results/Nitrogen/") %>%
  str_replace(".xlsx", "")

#combine into one dataframe and rename
#"Stickstoff (N) (%)"

nutrient.content.N.all.dates <-
  bind_rows(nutrient.content.N.by.date, .id = "Date") %>%
  `colnames<-`(c("Index", "Date", "Treatment", "Cultivar",
                 "N"))

#check variable classes
sapply(nutrient.content.N.all.dates, class)
nutrient.content.N.all.dates$Date <-
  dmy(nutrient.content.N.all.dates$Date)

## Combine both N and P ==================================
Long.all.dates.N <-
  nutrient.content.N.all.dates %>%
  pivot_longer(cols = N,
               names_to = "Nutrient",
               values_to = "Concentration")
Long.all.dates.P <-
  nutrient.content.P.all.dates %>%
  pivot_longer(cols = P:Si,
               names_to = "Nutrient",
               values_to = "Concentration")

##Greenhouse experimental setup info ==============================
# Load Treatment information: which Tree has which Treatment?
# NB!!!! Tree number must always be the first  in the
# sample name (i.e. 4_APP_N and not APP_4_N etc)
Experimental_information <-
  read_excel("../Experimental_information.xlsx")

Treatment_key <-
  Experimental_information %>%
  select(TreeID, Treatment) %>%
  mutate(TreeID = as.factor(TreeID)) %>%
  mutate(Treatment = gsub("extra", "cont", Treatment)) #extra trees are classed as controls

Long.all.dates <-
  bind_rows(Long.all.dates.N, Long.all.dates.P) %>%
  separate(Treatment, into = c("TreeID", NA, NA)) %>%
  mutate(TreeID = as.factor(TreeID)) %>%
  left_join(Treatment_key) %>%
  relocate(Treatment, .after = TreeID) %>%
  mutate(Treatment = str_replace(Treatment, "extra", "cont"))


##Plotting =======

#visualise nutrient data (Y)
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 3 * IQR(x) |
           x > quantile(x, 0.75) + 3 * IQR(x))
}


Long.all.dates <-
  Long.all.dates %>% group_by(Nutrient, Date) %>%
  mutate(outlier = ifelse(
    is_outlier(Concentration),
    as.numeric(Concentration),
    as.numeric(NA)
  ))

library(ggsci)


plot.nutrients.Date.x <- function(data) {
  ggplot(data = data, aes(x = as.factor(Date), y = Concentration)) +
    geom_boxplot(aes(fill = Treatment),
                 alpha = 0.4,
                 outlier.size = -1) +
    geom_point(aes(colour = Treatment),
               position = position_jitterdodge(jitter.width = 0.2),
               size = 1) +
    geom_point(
      aes(y = outlier, fill = Treatment),
      position = position_dodge(width = 0.67),
      shape = 4,
      colour = "red",
      stroke = 1.3,
      show.legend = F
    ) +
    #geom_text(aes(label = outlier, group = Date), position = position_dodge(width = 0.9))+
    #geom_violin(aes(fill = as.factor(Date))) +
    #geom_jitter(aes(shape = as.factor(Date)),
    # size = 1.5,
    # position = position_dodge2(width = 0.8))+
    #geom_hline(aes(yintercept = threshold), thresholds)+
    facet_wrap(~ Nutrient, scales = "free", nrow = 2) +
    theme(axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1.2
    ),
    text = element_text(size = 28)) +
    theme_classic() +
    ylab("Concentration (%)") +
    xlab("Date") +
    scale_fill(
      limits = c("cont", "APP", "N", "LN", "P", "MoP"),
      labels = c("Control", "APP", "-N", "Low N", "-P", "Low P"),
      name = "Treatment"
    ) +
    scale_colour(
      limits = c("cont", "APP", "N", "LN", "P", "MoP"),
      labels = c("Control", "APP", "-N", "Low N", "-P", "Low P"),
      name = "Treatment"
    ) +
    scale_shape_discrete(
      limits = c("cont", "APP", "N", "LN", "P", "MoP"),
      labels = c("Control", "APP", "-N", "Low N", "-P", "Low P")
    ) +
    theme(text = element_text(size = 18),
          legend.key.size = unit(1, 'cm'))
}


(boxplot.with.outliers <-
    plot.nutrients.Date.x(
      data = dplyr::filter(Long.all.dates, Nutrient %in% c("N", "P")) %>%
        mutate(Treatment = factor(
          Treatment,
          levels = c("cont", "APP", "N", "LN", "P", "MoP"),
          ordered = T
        ))
    ))
Long.all.dates$Treatment %>% is.ordered

#remove outliers
Long.all.dates %>% filter(is.na(outlier)) -> Long.all.dates #remove outliers

data = Long.all.dates
data = dplyr::filter(data, Nutrient %in% c("N", "P")) %>%
  mutate(
    Treatment = factor(
      Treatment,
      levels = c("cont", "APP", "N", "LN", "P", "MoP"),
      ordered = T
    ) %>%
      fct_recode(
        Control = "cont",
        APP = "APP",
        `-N` = "N",
        `Low N` = "LN",
        `-P` = "P",
        `Low P` = "MoP"
      )
  ) %>%
  arrange(factor(Treatment, levels = c(
    "Control", "APP", "-N", "Low N", "-P", "Low P"
  ))) %>%
  mutate(transformed = (Concentration))


#Data calculations: useful to dig deeper
library(ggpubr)
compare_means(
  transformed ~ Treatment,
  group.by = c("Nutrient", "Date"),
  method = "anova",
  data = data
)

UQs <-  data %>% group_by(Date, Treatment, Nutrient) %>%
  summarise(UQ = quantile(Concentration, .75),
            IQR = IQR(Concentration)) %>%
  mutate(margin = ifelse(Nutrient == "N", 0.54, 0.085))
data %>% distinct(Nutrient, Date) %>% mutate(group2 = "Control", p.signif = NA) -> controls
data %>% mutate(Treatment = factor(Treatment, ordered = F)) %>% group_by(Nutrient, Date) %>%
  do(means =  compare_means(
    Concentration ~ Treatment,
    method = "t.test",
    ref.group = "Control",
    data = .data
  )) %>%
  unnest(means) %>% bind_rows(controls) %>%  mutate(Treatment = factor(
    group2,
    levels = c("Control", "APP", "-N", "Low N", "-P", "Low P"),
    ordered = T
  )) %>%
  arrange(Nutrient, Date, Treatment) %>% left_join(UQs, by = c("Treatment", "Date", "Nutrient")) -> means

#manual curation: add significance to plot
means[which(means$Nutrient == "N" &
              means$Date == "2023-06-30" &
              means$Treatment == "Low N"), "margin"] <- 0.7
means[which(means$Nutrient == "N" &
              means$Date == "2023-07-10" &
              means$Treatment == "Low P"), "margin"] <- 0.7
means[which(means$Nutrient == "P" &
              means$Date == "2023-06-09" &
              means$Treatment == "APP"), "margin"] <- 0.087
means[which(means$Nutrient == "P" &
              means$Date == "2023-06-09" &
              means$Treatment == "-P"), "margin"] <- 0.06
means[which(means$Nutrient == "P" &
              means$Date == "2023-07-10" &
              means$Treatment == "-N"), "margin"] <- 0.15

boxplot.with.outliers +
  geom_text(
    data = means,
    aes(
      y = UQ + margin,
      x = as.factor(Date),
      fill = Treatment,
      label = p.signif
    ),
    #position = position_jitterdodge(dodge.width = 0.6, jitter.width = 0, jitter.height = 1),
    position = position_dodge(width = .75),
    size = 3.5
  ) +
  theme(text = element_text(size = 12)) -> boxplot.with.outliers


#all in one row
boxplot.with.outliers +
  geom_text(
    data = means,
    aes(
      y = UQ + margin,
      x = as.factor(Date),
      fill = Treatment,
      label = p.signif
    ),
    #position = position_jitterdodge(dodge.width = 0.6, jitter.width = 0, jitter.height = 1),
    position = position_dodge(width = .75),
    size = 3.5
  ) +
  theme(text = element_text(size = 12))+
  facet_wrap(~Nutrient, ncol = 2, scales = "free") -> x

x$layers[[2]]$aes_params$size <- .8
x
# check assumptions
library(DescTools)
library(car)

data  %>% group_by(Nutrient, Date) %>% 
  do(anova = lm(transformed ~ Treatment, data = .)) ->
  tests
tests$anova %>% lapply(plot, ask = F)

## Shoot Growth ==========================
#import data
library(readxl)
library(zoo)

ShootGrowth <- read_excel("../ShootGrowth2023.xlsx") %>%
  select(-ShootOrder) %>%   #remove and readd more cleverly
  na.locf() %>% #fill in Missing values
  group_by(TreeID) %>%  mutate(ShootOrder = row_number()) %>% #shoot number
  left_join(
    Experimental_information %>% select(TreeID, Treatment) %>%
      mutate(TreeID = as.numeric(TreeID)),
    by = "TreeID"
  ) %>%
  mutate(Treatment = gsub("extra", "cont", Treatment))


###      Total Growth    ===================

#Mean shoot growth
ShootGrowth %>%  group_by(Treatment) %>%
  summarise(MeanShootGrowth = sum(Length) / n()) %>% arrange(desc(MeanShootGrowth))

#Keeping tree information for std
ShootGrowth %>%  group_by(TreeID, Treatment) %>%
  summarise(TotalShootGrowth = sum(Length))  %>%
  mutate(
    Treatment = factor(
      Treatment,
      levels = c("cont", "APP", "N", "LN", "P", "MoP"),
      ordered = T
    ) %>%
      fct_recode(
        C = "cont",
        APP = "APP",
        `-N` = "N",
        `Low N` = "LN",
        `-P` = "P",
        `Low P` = "MoP"
      )
  ) %>%
  arrange(factor(Treatment, levels = c("C", "APP", "-N", "Low N", "-P", "Low P"))) ->
  TotalShootGrowth

TotalShootGrowth
ggplot(data = TotalShootGrowth, aes(x = Treatment, y = log(TotalShootGrowth *
                                                             1000))) +
  geom_boxplot(aes(fill = Treatment),
               alpha = 0.4,
               outlier.size = -1) +
  geom_point(aes(colour = Treatment),
             position = position_jitter(width = 0.2),
             size = 2) +
  theme_classic() +
  #stat_compare_means(method = "anova", label.y.npc = "bottom", label.x.npc = "left", label = "p.format")+
  stat_compare_means(
    method = "t.test",
    ref.group = "C",
    label = "p.signif",
    label.y.npc = 0.8,
    size = 3.5
  ) +
  scale_fill() +
  scale_colour() +
  ylab("Log(Total Shoot Growth (mm))") +
  theme(text = element_text(size = 12)) -> shoot.growth.boxplot
(
  nutrient.boxplots <-
    ggpubr::ggarrange(
      shoot.growth.boxplot,
      boxplot.with.outliers,
      nrow = 1,
      legend = "right",
      labels = "auto",
      widths = c(2, 4),
      common.legend = T
    )
)

###  Growth Distribution  =================================

ShootGrowth %>% select(TreeID, Treatment) %>% distinct() %>%
  group_by(Treatment) %>% summarise(trt.count = n()) -> trt.count

ShootGrowth %>% mutate(bin = cut(Length, breaks = c(seq(0, 300, 10)))) %>%
  group_by(Treatment, bin) %>% summarise(Cum.length = sum(Length)) %>%
  left_join(trt.count, "Treatment") %>% mutate(Cum.length.tree = Cum.length /
                                                 trt.count) -> length.dist

ggplot(length.dist, aes(x = bin, y = Cum.length.tree)) +
  geom_density(
    aes(
      colour = Treatment,
      group = Treatment,
      fill = Treatment
    ),
    linewidth = 1.2,
    alpha = 0.2,
    stat = "identity"
  ) +
  theme_classic() +
  scale_fill(
    limits = c("cont", "APP", "N", "LN", "P", "MoP"),
    labels = c("Control", "APP", "-N", "Low N", "-P", "Low P"),
    name = "Treatment"
  ) +
  scale_colour(
    limits = c("cont", "APP", "N", "LN", "P", "MoP"),
    labels = c("Control", "APP", "-N", "Low N", "-P", "Low P"),
    name = "Treatment"
  )
#facet_wrap(~Treatment)



##Spectral Data ======================
#                      All of this is data wrangling
# Combine Nutrient data and mean spectra according to the labels we
# took in the field (i.e. which scan belongs to each tree). You can
# just load the final data set at the end.

#load spectral data
library(spectrolab)

# sed files must be saved directly in each months folder (no subfolders)
setwd(dirname(getActiveDocumentContext()$path))
setwd("../SED files/")
spectra.by.date <-
  lapply(dir_ls(), function(working.directory) {
    setwd(working.directory)
    #list of all sig files
    file.list <- list.files(pattern = "*.sed")
    spectra <- read_spectra(file.list, extract_metadata = T)
    setwd("../")
    return(spectra)
  })

metadata <-
  lapply(spectra.by.date, function(spectra) {
    metadf <- meta(spectra)
    return(list("dims" = dim(spectra), "metadata" = metadf))
  })
library(janitor)
#total spectra
metadata %>% get_elements("dims") %>% bind_rows(.id = "Date") %>% select(n_samples)
#data loaded correctly, continue working in main directory:
setwd(dirname(getActiveDocumentContext()$path))
#reducing the scan down to simple number and setting classes

df.by.date <-
  lapply(spectra.by.date, function(spectra) {
    # spectra = spectra.by.date$`2023_Jul_10`
    df <-
      as.data.frame(spectra) %>%
      separate(sample_name, c(NA, "Experiment", "x1", "Target", "filetype")) %>%
      rename(Scan = Target) %>%
      select(-x1)
    df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
    df$Time <-
      format(as.POSIXlt(df$Time, format = "%H:%M:%S"), "%H:%M:%S")
    df$Scan <- gsub("^0+", "", df$Scan)
    return("df" = df) #dimensions df)
  })


##     Labels =======================================


#import.labels: NB! must have one file for each date. All files should be saved
#under the date in the form 2023_May_30

label.files <- dir_ls("../Excel trees/") %>%
  `names<-`(names(df.by.date)) #NB! the order of the data frames must be exactly the same as label files
label.files %>% names()

#colnames must all be the the same: Scan, Nutrient, TreeID, CIF (although not
#really needed)
labels.by.date <-
  lapply(label.files, function(label.file) {
    read_excel(label.file)
  })

#labels for which we don't have scans
check.scans.vs.labels <-
  function(labels, df) {
    labels %>%
      filter(!Scan %in% df$Scan) %>%   #select rows in which the label scan number is not in the spectra: eg. May 30 scans are not all in the same folder. change so that it is the case.
      select(Scan) %>%
      mutate(Scan = as.numeric(Scan)) %>%
      arrange(Scan)
  } # NAs are errors
mapply(check.scans.vs.labels, labels.by.date, df.by.date)
#check we have no labels with with scan id's that are not also in the scan data. i.e. we have a label for every scan: easier if the data are clean (table scan numbers stop
# when the actual scans do)

#we are missing scans 103 to 255 from 2 May: Data lost to a technical
# malfunctions

#scans for which we don't have labels
check.labels.vs.scans <-
  function(df, labels) {
    df %>%
      filter(!Scan %in% labels$Scan) %>% #Select scans in spectral data that are not in labels
      select(Scan)
    #nrow() # . = the amount of errors and refs i.e. labels and scans are congruent
  }
mapply(check.labels.vs.scans, df.by.date, labels.by.date)
#scans 1 and 2 from 30 June must have been errors or not relevant.
#Not in labels merge labels and scans and calculate
#tree/leaf means for each nutrient

calculate_mode <-
  function(x) {
    # function for aggregating tree treatment class
    uniqx <- unique(na.omit(x))
    uniqx[which.max(tabulate(match(x, uniqx)))]
  }


#merge labels and scans (means
#spectra for P estimation are also done here)
merge.labels.and.scans <- function(labels, scans) {
  labels$Scan <- as.character(labels$Scan)
  scans$Scan <- as.character(scans$Scan)
  df.arithmetic.mean <- inner_join(labels, scans, by = "Scan") %>%
    group_by(TreeID, Nutrient) %>%
    summarise(across(Scan:Channels, calculate_mode),
              across(`350`:`2500`, mean))
  library(psych)
  df.harmonic.mean <- inner_join(labels, scans, by = "Scan") %>%
    group_by(TreeID, Nutrient) %>%
    summarise(across(Scan:Channels, calculate_mode),
              across(`350`:`2500`, harmonic.mean))
  
  df.geometric.mean <- inner_join(labels, scans, by = "Scan") %>%
    group_by(TreeID, Nutrient) %>%
    summarise(across(Scan:Channels, calculate_mode),
              across(`350`:`2500`, geometric.mean))
  
  return(
    list(
      "arithmetic" = df.arithmetic.mean,
      "harmonic" = df.harmonic.mean,
      "geometric" = df.geometric.mean
    )
  )
  
  df$Treatment <- gsub("extra", "cont", df$Treatment)
}
df.by.date <-
  mapply(merge.labels.and.scans, labels.by.date, df.by.date)
df.all.arithmetic <-
  df.by.date["arithmetic", ] %>% bind_rows(.id = "Date1") %>% relocate("Date1")
df.all.harmonic <-
  df.by.date["harmonic", ] %>% bind_rows(.id = "Date1") %>% relocate("Date1")
df.all.geometric <-
  df.by.date["geometric", ] %>% bind_rows(.id = "Date1") %>% relocate("Date1")
df.all <- bind_rows(
  list(
    "arithmetic" = df.all.arithmetic,
    "harmonic" = df.all.harmonic,
    "geometric" = df.all.geometric
  ),
  .id = "Mean"
) %>%
  ungroup() %>%
  filter((Mean == "arithmetic" &
            Nutrient == "N") | Nutrient == "P")


# #N contains just N and P contains all other nutrients. Therefore we must work with different data frames for N and P or with an impossibly long dataset
df.by.nutrient <- df.all %>% group_by(Nutrient) %>% group_split()
df.all %>% group_by(Nutrient) %>% group_keys()

#merge by date and tree ID.
Experimental_information <-
  Experimental_information %>% mutate(TreeID = as.character(TreeID))

nutrient.content.P.all.dates  <-
  Long.all.dates %>% filter(Nutrient != "N") %>%
  pivot_wider(names_from = Nutrient, values_from = Concentration) %>%
  select(-Treatment) %>%
  left_join(select(Experimental_information, TreeID, Treatment),
            by = "TreeID") %>%
  relocate(Treatment, .after = TreeID)

nutrient.content.N.all.dates <-
  Long.all.dates %>% filter(Nutrient == "N") %>%
  pivot_wider(names_from = Nutrient, values_from = Concentration) %>%
  select(-Treatment) %>%
  left_join(select(Experimental_information, TreeID, Treatment),
            by = "TreeID") %>%
  relocate(Treatment, .after = TreeID)

#bind df with nutrients: need to merge with dates
# NB!! dates in nutrients must match with dates of scans!
df.all %>% ungroup %>%
  select(Nutrient, Date, Date1) %>% distinct()
nutrient.content.N.all.dates %>% select(Index, Date) %>%
  distinct()
nutrient.content.P.all.dates %>% select(Index, Date) %>%
  distinct()

#merge df
df.N  <-
  df.by.nutrient[[1]] %>% inner_join(nutrient.content.N.all.dates,
                                     by = c("TreeID", "Date")) %>%
  relocate(CIF:N, .after = Nutrient)

df.P <-
  df.by.nutrient[[2]] %>% inner_join(nutrient.content.P.all.dates,
                                     by = c("TreeID", "Date")) %>%
  relocate(CIF:Si, .after = Nutrient)


# do a quick qaulity control check: Manually check that scan on a
# date matches the TreeID and that TreeID on a specific date matches the nurients content recorded.
df.N %>% select(Scan, Date, TreeID, Mean, N) %>%
  mutate(Scan = as.numeric(Scan)) #%>% View() # looks good!

df.P %>% select(Scan, Date, TreeID, Mean, P:Si) %>%
  mutate(Scan = as.numeric(Scan), TreeID = as.numeric(TreeID)) #%>% View() #also great!

saveRDS(list("N" = df.N, "P" = df.P), "dfs_GH_Nutrients_2023")

df.list <- list("N" = df.N, "P" = df.P) # list of dataframes for
# N and P: P has all other nutrients as well and thus both cannot be
#in the same dataframe.
detach("package:spectrolab", unload = T)


# Process data for spectral for analysis     ====================================



# hsdar::speclib: an R object that is specifically designed for  #
# spectral data. hsdar also has a lot of useful functions for    #
# processing and visualising data. Multiplicative Scatter        #
# Correction can only be done with PLS package                   #


# Download package tarball from CRAN archive

## installing hsdar (it was removed from CRAN)
# #install.packages("devtools")
# require(devtools)
#
# #rgdal is required for hsdar but is also archived:
# install_version("rgdal", version = "1.6-7", repos = "http://cran.us.r-project.org")
#
# #hsdar file details:
# url <-
#   "https://cran.r-project.org/src/contrib/Archive/hsdar/hsdar_1.0.4.tar.gz"
# pkgFile <- "hsdar_1.0.4.tar.gz"
# download.file(url = url, destfile = pkgFile)
#
# # Expand the zip file using whatever system functions are preferred
#
# # look at the DESCRIPTION file in the expanded package directory
#
# # Install dependencies list in the DESCRIPTION file
#
# install.packages(c("raster", "signal", "methods", "caret", "Boruta"))
#
# # Install package
# install.packages(pkgs = pkgFile, type = "source", repos = NULL)
#
# # Delete package tarball
# unlink(pkgFile)
#
# #installing hsdar
#
# library(rtools)
# url <-
#   "https://cran.r-project.org/src/contrib/Archive/hsdar/hsdar_1.0.4.tar.gz"
# install.packages(url, type = "source", repos = NULL)
#
#
#

library(hsdar)
library(pls)

# extra -> cont
df.list$N <-
  df.list$N %>%
  mutate(Treatment = gsub("extra", "cont", Treatment))

df.list$P <-
  df.list$P %>%
  mutate(Treatment = gsub("extra", "cont", Treatment))
#function to convert data frame to speclib
df.to.speclib <- function(df) {
  #everything that isn't spectra:
  #df = df.list$P
  SI <- df %>%
    dplyr::select(-c(`350`:`2500`)) %>%
    as.data.frame()
  
  spectra <- df %>%
    dplyr::select(c(`350`:`2500`))
  
  class(spectra)
  
  #create spectral library
  speclib <-
    speclib(as.matrix(spectra), wavelength = as.numeric(colnames(spectra)))
  
  #supplimentary data
  SI(speclib) <- SI
  
  return(speclib)
}

speclibs <- lapply(df.list, df.to.speclib)
#plot
par(mfrow = c(1, 1))

x <- speclibs[["N"]]
spectra(x) <- speclibs[["N"]] %>% spectra %>% scale()

y <- speclibs[["P"]]
spectra(y) <- speclibs[["P"]] %>% spectra %>% scale()
plot(speclibs[["N"]], main = "Spectra corresponding to N samples")
par(mfrow = c(1, 2))
speclibs[["P"]] %>% subset(Mean == "arithmetic",
                           main = "Spectra Corresponding to P samples") %>% plot()
speclibs[["P"]] %>% subset(Mean == "harmonic") %>% plot(main = "harmonic",
                                                        new = F,
                                                        col = "red")
speclibs[["P"]] %>% subset(Mean == "geometric") %>% plot(main = "geometric",
                                                         new = F,
                                                         col = "blue")

#Scale P to see the difference in means more clearly
y %>% subset(Mean == "arithmetic") %>% plot()
y %>% subset(Mean == "harmonic") %>% plot(main = "harmonic",
                                          new = F,
                                          col = "red")
y %>% subset(Mean == "geometric") %>% plot(main = "geometric",
                                           new = F,
                                           col = "blue")


#visualisations

#Plotting function

# functions to easily plot speclibs using ggplot
ggplot.spectra <-
  function(speclibrary, colour_by) {
    spectratoplot <- hsdar::spectra(speclibrary) %>%
      `colnames<-` (c(wavelength(speclibrary))) %>%
      cbind(SI(speclibrary)) %>%
      as_tibble() %>%
      relocate(colnames(SI(speclibrary)))
    
    dfe <- gather(spectratoplot,
                  band,
                  reflectance,
                  as.character(wavelength(speclibrary)),
                  factor_key = TRUE)
    
    ggplot(data = dfe) +
      geom_line(aes(
        x = as.numeric(as.character(band)),
        y = as.numeric(reflectance),
        colour = {
          {
            colour_by
          }
        },
        group = Scan
      )) +
      theme_classic()
    #scale_colour_manual(values = pal)
    #scale_color_viridis(option = "D")
  }

ggplot.spectra.mean <-
  function(speclibrary, colour_by, linewidth = 1.2, axiswidth = 1) {
    # speclibrary = speclibs$N
    # colour_by = SI(speclibs$N)$Treatment
    spectratoplot <- hsdar::spectra(speclibrary) %>%
      `colnames<-` (c(wavelength(speclibrary))) %>%
      cbind(SI(speclibrary)) %>%
      as_tibble() %>%
      relocate(colnames(SI(speclibrary)))
    
    dfe <- tidyr::gather(spectratoplot,
                         band,
                         reflectance,
                         as.character(wavelength(speclibrary)),
                         factor_key = TRUE) %>%
      group_by(band, {
        {
          colour_by
        }
      }) %>%
      summarise(Mean = mean(reflectance), SD = sd(reflectance)) %>%
      mutate(band = as.numeric(as.character(band)))
    
    ggplot(data = dfe) +
      geom_ribbon(aes(
        x = band,
        y = Mean,
        ymin = Mean - SD,
        ymax = Mean + SD,
        fill = {
          {
            colour_by
          }
        }
      ),
      alpha = .2) +
      geom_line(aes(x = band,
                    y = Mean,
                    colour = {
                      {
                        colour_by
                      }
                    }),
                linewidth = linewidth) +
      theme_classic() +
      scale_fill(
        limits = c("cont", "APP", "N", "LN", "P", "MoP"),
        labels = c("Control", "APP", "-N", "Low N", "-P", "Low P"),
        name = "Treatment"
      ) +
      scale_colour(
        limits = c("cont", "APP", "N", "LN", "P", "MoP"),
        labels = c("Control", "APP", "-N", "Low N", "-P", "Low P"),
        name = "Treatment"
      ) +
      xlab("Wavelength (nm)")+
      theme(line = element_line(linewidth = axiswidth))
    #scale_colour_manual(values = pal)
    #scale_color_viridis(option = "D")
  }


a <-
  ggplot.spectra.mean(speclibs$N, colour_by = Treatment, linewidth = 0.5) +
  ylab("Reflectance")
b <- ggplot.spectra.mean(speclibs$P, colour_by = Treatment, linewidth = 0.5) +
  ylab("Reflectance")

ggpubr::ggarrange(
  a,
  b,
  labels = "auto",
  common.legend = T,
  nrow = 2,
  ncol = 1,
  align = "v",
  legend = "right"
)

## Outlier Removal ===============================================
#   Outlier spectra are removed by spectral angles greater than 3  #
# times greater than the mean spectral angle                       #


outlier.function <- function(speclib) {
  #speclib = speclibs$N
  ref = apply(speclib, FUN = mean) #calculate mean spectrum
  plot(ref) #plot to check normal
  spec_ang <- sam(speclib, ref = ref) #calculate spectral angles
  print(
    ggplot(data = cbind(spec_ang, SI(speclib)),
           y = spec_ang) +
      geom_histogram(aes(x = scale(spec_ang))) +
      geom_vline(xintercept = 3 * sd(scale(spec_ang))) +
      xlab("Normalised Spectral Angle to Mean Spectra") +
      geom_vline(xintercept = mean(scale(spec_ang)) + 3 * sd(scale(spec_ang)))
  )
  ol <- which(spec_ang > mean(spec_ang) + 3 * sd(spec_ang))
  print(ol)
  
  print(ggplot.spectra(speclib[ol], Treatment))
  # print(ggplot.spectra(speclib, Treatment))
  # print(ggplot.spectra(speclib[-ol], Treatment))
  
  for (i in ol) {
    print(plot(
      speclib,
      FUN = mean,
      new =  T,
      ylim = c(0, 0.65),
      main = paste(i)
    ))
    print(plot(
      speclib,
      FUN = i,
      new = F,
      col = 'red',
      
    ))
  }
  print(plot(speclib, FUN = -ol))
  return(ol)
}

par(mfrow = c(1, 1))
outliers <- lapply(speclibs, outlier.function)

# #remove outliers:
speclibs$N <- speclibs$N[-outliers$N] #do once and not again
speclibs$P <- speclibs$P[-outliers$P]


#remove noisy bit in the beginning (first 30 bands)
plot(speclibs$P[, 1:50])
plot(speclibs$N[, 1:50])
speclibs$N <- speclibs$N[,-c(1:15)] # do this once and not again
speclibs$P <- speclibs$P[,-c(1:15)]
plot(speclibs$P[, 1:50])
plot(speclibs$N[, 1:50])

## preprocess spectra: =======================================
# MSC, first deriviative, second derivative and veg indices
preprocess.spectra <- function(speclib, linewidth = 1) {
  # speclib = speclibs$N
  #create MSC of spectra
  msc.spectra <- msc(spectra(speclib))
  speclib.msc <- speclib
  spectra(speclib.msc) <- as.data.frame(msc.spectra)
  
  Raw <-
    ggplot.spectra.mean(speclib, colour_by = Treatment, linewidth) +
    ylab("Reflectance (Raw)")
  MSC <-
    ggplot.spectra.mean(speclib.msc, colour_by = Treatment, linewidth) +
    ylab("MSC-Reflectance")
  
  #Prepare data for derivative
  #smooth <- noiseFiltering(speclib, method = "sgolay", n = 9) #smoothing
  smooth <- speclib #no smooth
  #First derivative
  dR.dwl <- derivative.speclib(smooth, m = 1)
  D1 <-
    ggplot.spectra.mean(dR.dwl, colour_by = Treatment, linewidth) +
    ylab("First Derivative Reflectance")
  
  #Second derivative
  smooth <- noiseFiltering(speclib, method = "sgolay", n = 25)
  smooth <- speclib #no smooth
  dR.dwl2 <-  derivative.speclib(smooth, m = 2)
  D2 <-
    ggplot.spectra.mean(dR.dwl2, colour_by = Treatment, linewidth) +
    ylab("Second Derivative Reflectance")
  
  #spectral indices
  vis <- vegindex()
  vi <- vegindex(speclib, index = vis) %>%
    as_tibble()
  
  full <-
    cbind(SI(speclib.msc),
          spectra(speclib.msc),
          spectra((dR.dwl)),
          spectra(dR.dwl2),
          vi) %>%
    `colnames<-`(c(
      colnames(SI(speclib)),
      wavelength(speclib),
      paste("d", wavelength(dR.dwl), sep = ""),
      paste("dd", wavelength(dR.dwl2), sep = ""),
      colnames(vi)
    ))
  plots <- list(
    "Raw" = Raw,
    "MSC" = MSC,
    "D1" = D1,
    "D2" = D2
  )
  data_sets <- list(
    #"full" = full, #full is too much to handle
    "msc" = cbind(SI(speclib.msc), as.data.frame(speclib.msc)),
    "d1" = cbind(SI(dR.dwl), as.data.frame(dR.dwl)),
    "d2" = cbind(SI(dR.dwl2), as.data.frame(dR.dwl2)),
    "vi" = cbind(SI(speclib), as.data.frame(vi))
  )
  return(list("data_sets" = data_sets,
              "spectra" = plots))
}

data_sets <- lapply(speclibs, preprocess.spectra)
ggpubr::ggarrange(
  plotlist = data_sets$N$spectra,
  common.legend = T,
  legend = "right",
  align = "hv",
  labels = "auto"
)

#all in one row
ggpubr::ggarrange(
  plotlist = data_sets$N$spectra,
  common.legend = T,
  legend = "right",
  align = "hv",
  labels = "auto",
  ncol = 4
)
#dimensions
select <- dplyr::select
filter <- dplyr::filter
data_sets$N[["data_sets"]][["msc"]] %>% select(N) -> N
(range(N) %>% diff()) / mean(N$N)

data_sets$P[["data_sets"]][["msc"]] %>% dplyr::filter(Mean == "arithmetic") %>% select(P) %>% na.omit -> P
(range(P) %>% diff()) / mean(P$P, na.rm = T)

# saveRDS(data_sets, "data_sets")
#data_sets <- readRDS("data_sets")


#   Training and Test Sets =========================================
library(janitor)
library(caTools)



data <- get_elements(data_sets, "data_sets")
trees <-
  data %>% unlist(F) %>%  bind_rows() %>% dplyr::select(TreeID) %>% unique() %>% mutate(TreeID = as.numeric(TreeID)) %>% arrange(TreeID) %>% rename(training_tree = TreeID)
trees <- trees$training_tree
trees == 1:150
split <-
  sample.split(levels(as.factor(trees)), SplitRatio = .8) # Split trees into training and test

training_trees <- trees[split == T] %>%
  as.data.frame() #split the trees into 3/4 training and 1/4 test
colnames(training_trees) <- "TreeID"
training_trees <-
  training_trees %>%  mutate(TreeID = as.character(TreeID))

# saveRDS(training_trees, "training_trees")

#there seems to be a problem here where setting the seed does not ensure identical
# results. To replicate the data in the article use the training and test sets
# saved as RDS objects in the appropriate folder
training_trees <- readRDS("training_trees")

test_trees <-
  trees %>% as.data.frame() %>% `colnames<-`("TreeID") %>%
  dplyr::filter(!(TreeID %in% training_trees$TreeID))
test_trees <-
  test_trees %>%  mutate(TreeID = as.character(TreeID))

# saveRDS(test_trees, "test_trees")

test_trees <- readRDS("test_trees")

# check distribution
training_trees %>% left_join(Experimental_information, by = "TreeID") %>% dplyr::select(Treatment) %>% distinct()
test_trees %>% left_join(Experimental_information, by = "TreeID") %>% dplyr::select(Treatment) %>% distinct()
training_sets.N <-
  lapply(data$N, function(dataset) {
    right_join(dataset, training_trees, by = "TreeID")
  })
training_sets.P <-
  lapply(data$P, function(dataset) {
    right_join(dataset, training_trees, by = "TreeID")
  })
test_sets.N  <-
  lapply(data$N, function(dataset) {
    right_join(dataset, test_trees, by = "TreeID")
  })
test_sets.P  <-
  lapply(data$P, function(dataset) {
    right_join(dataset, test_trees, by = "TreeID")
  })

training_sets <- list("N" = training_sets.N,
                      "P" = training_sets.P)
test_sets <- list("N" = test_sets.N,
                  "P" = test_sets.P)



#Functions to partition data into different sets.

#create divide the data into the data from each Date, and each mean
# (Arithmetic, Geometric and harmonic)
partition.sets <- function(dataset) {
  dataset %>% group_by(Date, Mean) %>% group_split()
}

# keys so that we can see which set is what
set.keys <- function(dataset) {
  dataset %>% group_by(Date, Mean) %>% group_keys
}

training_sets_bydateXmean <-
  lapply(training_sets, function(list) {
    lapply(list, partition.sets)
  })

keys.training.bymeanxdate <-
  lapply(training_sets, function(list) {
    lapply(list, set.keys)
  })

# Data pooled across the whole year (devided only by mean)
training_sets_bymean <- lapply(training_sets, function(list) {
  lapply(list, function(dataset) {
    dataset %>% group_by(Mean) %>% group_split()
  })
})

keys.training.by.mean <- lapply(training_sets, function(list) {
  lapply(list, function(dataset) {
    dataset %>% group_by(Mean) %>% group_keys
  })
})


# now do the same thing for the test sets
test_sets_bydateXmean <- lapply(test_sets, function(list) {
  lapply(list, partition.sets)
})

keys.test <- lapply(test_sets, function(list) {
  lapply(list, set.keys)
})
test_sets_byMean <- lapply(test_sets, function(list) {
  lapply(list, function(dataset) {
    dataset %>% group_by(Mean) %>% group_split()
  })
})

## training columns ===================================
#select the columns that will be used to train the models
training.vs.info.columns <- function(datasetbyfactor) {
  # datasetbyfactor <- training_sets_bydateXmean
  PLS.columns.spectra <- function(dataset, response) {
    dataset %>%
      dplyr::select({
        {
          response
        }
      }, `365`:last_col()) %>%
      na.omit()
  }
  PLS.columns.vi <- function(dataset, response) {
    dataset %>%
      dplyr::select({
        {
          response
        }
      }, Boochs:last_col()) %>%
      na.omit()
  }
  training.sets.N.spectra <-
    lapply(datasetbyfactor$N[1:3], function(list) {
      #change 3 to 4! for full dataset
      lapply(list, PLS.columns.spectra, response = N)
    })
  training.sets.P.spectra <-
    lapply(datasetbyfactor$P[1:3], function(list) {
      lapply(list, PLS.columns.spectra, response = P)
    })
  training.sets.N.vi <-
    lapply(datasetbyfactor$N[4], function(list) {
      #change 4 to 5! for full dataset
      lapply(list, PLS.columns.vi, response = N)
    })
  training.sets.P.vi <-
    lapply(datasetbyfactor$P[4], function(list) {
      lapply(list, PLS.columns.vi, response = P)
    })
  
  
  PLS.training.columns <-
    list(
      "N" = c(training.sets.N.spectra, training.sets.N.vi),
      "P" = c(training.sets.P.spectra, training.sets.P.vi)
    )
  info.columns <- function(dataset) {
    dataset %>%
      dplyr::select(Mean:Channels)
  }
  
  training.info <- lapply(datasetbyfactor, function(list1) {
    lapply(list1, function(list2) {
      lapply(list2, info.columns)
    })
  })
  return(list("training_columns" = PLS.training.columns ,
              "info_columns" = training.info))
}

training.sets.columns <-
  lapply(
    list("byDateXmean" = training_sets_bydateXmean,
         "byMean" = training_sets_bymean),
    training.vs.info.columns
  )

#       PLS     ==================================================

library(job)
library(caret)

#PLS

#create a train function that can be passed over a list of training sets
PLS.train <- function(dataset) {
  dataset <- as.data.frame(dataset)
  ctrl <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 3,
    verboseIter = TRUE
  )
  
  PLS <- caret::train(
    y = dataset[, 1],
    x = dataset[, -1],
    method = 'pls',
    tuneLength = 50,
    verboseIter = T,
    preProcess = c("center", "scale"),
    trControl = ctrl
  )
}


# running the job will take considerable time, and produce slightly
# different results (setting the seed did not seem to do what it
# is supposed to).
# to replicate our results exactly  load the trained model saved
# as an RDS file.
# ATTENTION! Running the job with the saveRDS line uncommoneted,
# will overwrite this file.

library(job)
job({
  trained.models.pls.all.dates <-
    lapply(training.sets.columns$byMean$training_columns, function(list2) {
      lapply(list2, function(list1) {
        lapply(list1, PLS.train)
      })
    })
  saveRDS(trained.models.pls.all.dates,
          "trained.models.pls.all.dates")
}, import = "auto") #trained.models.pls is too large we will have to find another solution: find best preprocessing using a single full dataset

# trained.models.pls.all.dates <-
#   readRDS("trained.models.pls.all.dates")
gc()
#View(trained.models.pls.all.dates)

#summarise into one data frame
create.data.frame.results <- function(preprocess) {
  #preprocess <- "msc"
  x <- keys.training.by.mean %>% bind_rows()
  results <-
    trained.models.pls.all.dates %>% get_elements("results")  %>%
    bind_rows(.id = "id") %>%   mutate(id = paste0(id, ".", x[[preprocess]]$Mean))
  results <-
    results[[preprocess]] %>% `names<-`(results$id) %>% bind_rows(.id = "id") %>%
    separate(id, into = c("Nutrient", "Mean"))
}

results <- lapply(c(
  "msc" = "msc",
  "d1" = "d1",
  "d2" = "d2",
  "vi" = "vi"
),
create.data.frame.results) %>% bind_rows(.id = "id")

trained.models.pls.all.dates$N$msc[[1]] %>% class()
(
  best.preprocessing <-
    results %>% mutate(is.vi = id == "vi") %>%  relocate(is.vi, .after = id) %>%
    group_by(Nutrient, is.vi) %>%
    slice_min(RMSE, n = 1)
) # the best combination of preprocessing
results %>% group_by(Nutrient) %>% slice_max(Rsquared, n = 1) #this agrees
best.results <-
  results %>% group_by(Nutrient, id, Mean) %>% slice_min(RMSE, n = 1) #for report

#get ranges of results
best.results %>% group_by(Nutrient, Mean) %>% summarise(across(RMSE:MAE, min)) -> mins
best.results %>% group_by(Nutrient, Mean) %>% summarise(across(RMSE:MAE, max)) -> maxs
ranges <- inner_join(mins,
                     maxs,
                     by = c("Nutrient", "Mean"),
                     suffix = c(".min", ".max")) %>%
  select(Nutrient,
         Mean,
         Rsquared.min,
         Rsquared.max,
         RMSE.min,
         RMSE.max,
         MAE.min,
         MAE.max)


(cv.plots <-
list(
  "N.msc" = trained.models.pls.all.dates$N$msc[[1]],
  "N.vi" = trained.models.pls.all.dates$N$vi[[1]],
  "P.msc.arithmetic" = trained.models.pls.all.dates$P$msc[[2]],
  "P.vi.arithmetic" = trained.models.pls.all.dates$P$vi[[2]]) %>% 
  get_elements("results") %>% bind_rows(.id = "model") %>% 
  separate(model, into = c("Nutrient", "Correction", "Mean")) %>% 
  ggplot(aes(x = ncomp, y = RMSE, fill = Correction, linetype = Correction)) +
  geom_line() +
  geom_ribbon(aes(
    ymax = RMSE + RMSESD,
    ymin = RMSE - RMSESD),
    alpha = 0.2) +
  facet_wrap(~Nutrient, scales = "free",
             labeller = labeller(
               Nutrient = c(
                 N = "N",
                 `P` = "P (Arithmetic Mean)"
             ))) +
  theme_classic() + 
  scale_fill(labels = c("MSC", "SVI"))+
  scale_linetype(labels = c("MSC", "SVI"))+
  labs(x = "Latent Variables",
       y = "RMSE (Repeated CV)")
)


ggplot(data = best.results, aes(x = id, y = RMSE, fill = Mean)) +
  geom_bar(stat = "identity",
           position = "dodge",
           alpha = 0.4) +
  geom_errorbar(
    aes(
      ymin = RMSE - RMSESD,
      ymax = RMSE + RMSESD,
      width = 0.5
    ),
    position = position_dodge(width = 0.9),
    colour = "black",
    linewidth = .5
  ) +
  facet_wrap( ~ Nutrient, scales = "free_y") +
  ylab("RMSE (CV)") +
  xlab("Preprocess Correction") +
  theme_classic() +
  scale_fill() +
  scale_x_discrete(labels = c("d1", "d2", "MSC", "SVI"))
  theme(legend.key.size = unit(1, 'cm')) +
  theme(text = element_text(size = 12))#for report


# The effect of mean is neglible. Random effects might change the outcome.
# arithmetic mean is used for simplicity.

best.models.all.dates <-
  list(
    "N.msc" = trained.models.pls.all.dates$N$msc[[1]],
    "N.vi" = trained.models.pls.all.dates$N$vi[[1]],
    "P.msc.arithmetic" = trained.models.pls.all.dates$P$msc[[1]],
    "P.vi.arithmetic" = trained.models.pls.all.dates$P$vi[[1]]
  )

best.test.byMean <- list(
  "N.msc" = test_sets_byMean$N$msc[[1]],
  "N.vi" = test_sets_byMean$N$vi[[1]],
  "P.msc.arithmetic" = test_sets_byMean$P$msc[[1]],
  "P.vi.arithmetic" = test_sets_byMean$P$vi[[1]]
)




#Testing Full Models -----------------------------------------------------

library(broom)

#Making predictions
y_pred <-
  mapply(predict, best.models.all.dates, best.test.byMean) %>% lapply(as_tibble) %>% bind_rows(.id = "id") %>% dplyr::rename(Predicted = value)
(
  data <-
    best.test.byMean %>% bind_rows() %>% bind_cols(y_pred)  %>%
    dplyr::select(c(id, Date, N, P, Treatment, Predicted, TreeID)) %>%
    separate(id, into = c(NA, "Correction")) %>%
    pivot_longer(c(N, P), names_to = "Nutrient") %>%
    na.omit() %>%
    mutate(Correction = fct_recode(
      Correction, MSC = "msc", SVI = "vi"
    )) %>%
    group_by(Nutrient, Correction)
)


#calculate statistics
group_keys(data)
models <- lapply(data %>% group_split, function(dataset) {
  model <- lm(value ~ Predicted, data = dataset)
})
lapply(models, summary) %>% get_elements(c("coefficients"))
lapply(models, summary) %>% get_elements(c("r.squared"))
lapply(models, function(x) {
  rstandard(x) %>% as.numeric
}) %>% unlist() -> rstandard

data %>% ungroup() %>%  mutate(rstand = rstandard) %>% group_by(Nutrient, Correction) -> data
data %>% mutate(outlier = ifelse(abs(rstand) > 3, value, NA))  -> data

#linear mixed effects
library(lme4)
library(lmerTest)
library(broom.mixed)
models <-  data %>% group_by(Nutrient, Correction) %>% 
  do(Model = lmer(value ~ Predicted  +
                    (1 | Treatment/TreeID), data = .)) %>% 
  mutate(Summary = list(summary(Model))) 

models %>% 
  rowwise() %>%
  mutate(Tidy_Summary = list(tidy(Model))) %>%
  unnest(Tidy_Summary) ->
  summary.tibble 

summary.tibble  %>% 
  filter(term == "Predicted") %>% 
  select(Nutrient, Correction, term, estimate, Correction, p.value) %>% 
  mutate(p.adj = p.adjust(p.value, method = "holm"),
         p.format = p.adj %>% signif(2) %>% as.character) %>% 
  mutate(p.format = sub("e","%.% 10^",p.format)) ->
p.values

metrics <-
  data %>% group_by(Nutrient, Correction) %>%
  summarise(RMSE = caret::RMSE(Predicted, value),
            Rsquare = caret::R2(Predicted, value))

metrics.no.ol <-
  data %>% dplyr::filter(is.na(outlier)) %>% group_by(Nutrient, Correction) %>%
  summarise(RMSE = caret::RMSE(Predicted, value),
            Rsquare = caret::R2(Predicted, value))

#plot predictions against Observed
library(ggpubr)
library(ggh4x)

FacetEqualWrap <- ggproto(
  "FacetEqualWrap",
  FacetWrap,
  
  train_scales = function(self,
                          x_scales,
                          y_scales,
                          layout,
                          data,
                          params) {
    # doesn't make sense if there is not an x *and* y scale
    if (is.null(x_scales) || is.null(x_scales)) {
      stop("X and Y scales required for facet_equal_wrap")
    }
    
    # regular training of scales
    ggproto_parent(FacetWrap, self)$train_scales(x_scales, y_scales, layout, data, params)
    
    # switched training of scales (x and y and y on x)
    for (layer_data in data) {
      match_id <- match(layer_data$PANEL, layout$PANEL)
      
      x_vars <-
        intersect(x_scales[[1]]$aesthetics, names(layer_data))
      y_vars <-
        intersect(y_scales[[1]]$aesthetics, names(layer_data))
      
      SCALE_X <- layout$SCALE_X[match_id]
      ggplot2:::scale_apply(layer_data, y_vars, "train", SCALE_X, x_scales)
      
      SCALE_Y <- layout$SCALE_Y[match_id]
      ggplot2:::scale_apply(layer_data, x_vars, "train", SCALE_Y, y_scales)
    }
    
  }
)

facet_wrap_equal <- function(...) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super <- facet_wrap(...)
  
  ggproto(NULL,
          FacetEqualWrap,
          shrink = facet_super$shrink,
          params = facet_super$params)
}

(ggscatter(
  data = data,
  y = "value",
  x = "Predicted",
  shape = "Treatment",
  color = "Treatment"
) +  
    geom_point(
    aes(y = outlier),
    shape = 1,
    colour = "red",
    size = 4,
    stroke = 1.2
  ) +
  geom_abline() +
  #geom_text(aes(label = TreeID), vjust = 1.2, hjust = 1.2) +
  xlab("Predicted Nutrient Concentration (%)") +
  ylab("Measured Nutrient Concentration (%)") +
  facet_wrap_equal(Nutrient ~ Correction, scales = "free") +
  theme_classic() +
  scale_colour(
    limits = c("cont", "APP", "N", "P", "MoP"),
    labels = c("Control", "APP", "-N" , "-P","Low P")
  ) +
  scale_shape(
    limits = c("cont", "APP", "N", "P", "MoP"),
    labels = c("Control", "APP", "-N" , "-P","Low P")
  ) +
  stat_regline_equation(vjust = 2, aes(label =  ..adj.rr.label..)) +
  geom_text(data = p.values, aes(x = -Inf, y = Inf, 
                                 label = paste("italic(p) == ", p.format)), vjust = 5, hjust = -0.2,
            parse = T) +
  theme(text = element_text(size = 18)) ->
  test.plots)


#Paired t.test: is the difference between the error of the SVI model and MSC model statistically significant: no but close.
data %>% mutate(Err2 = (Predicted - value) ^ 2) %>% group_by(Nutrient) -> plotting.data

ggplot(plotting.data, aes(x = Correction, y = sqrt(Err2))) +
  geom_bar(stat = "summary") +
  geom_jitter(width = 0.3, aes(shape = Treatment, colour = Treatment)) +
  facet_wrap( ~ Nutrient, scales = "free")

plotting.data %>% group_by(Nutrient, Correction) %>% mutate(observation = row_number()) %>%
  filter(!(Nutrient == "P" & sqrt(Err2) > 0.1)) %>%
  pivot_wider(
    values_from = Err2,
    names_from = Correction,
    id_cols = c(Date, Nutrient, observation, Treatment)
  ) -> err.data

err.data %>% do(t.tests = t.test(Pair(sqrt(MSC), sqrt(SVI)) ~ 1, data = .data)) -> correction.err.comp
correction.err.comp[["t.tests"]]



### Relationship between N and P ------------------------------------------

# interesting: prediction does poorly on leaves with low N
# is P underestimated in trees absent of N: yes
library(dplyr)
data %>% filter(Nutrient == "P", Treatment == "N") %>% group_by(Correction) %>% do(t.test = t.test(Pair(value, Predicted) ~ 1, data = .data)) -> t.tests
t.tests$t.test

#is there a difference in the amount of underestimation between
# the SVI and MSC models: No.
t.test(Pair(value, Predicted) ~ Correction,
       data = data %>% filter(Nutrient == "P", Treatment == "N")) ->
  t.test.comparison
t.test.comparison


# by Date -------------------------------------------------

# reduce LV search space (each dataset is smaller)
PLS.train <- function(dataset) {
  dataset <- as.data.frame(dataset)
  ctrl <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 3,
    verboseIter = TRUE
  )
  
  PLS <- caret::train(
    y = dataset[, 1],
    x = dataset[, -1],
    method = 'pls',
    tuneLength = 20,
    verboseIter = T,
    preProcess = c("center", "scale"),
    trControl = ctrl
  )
}


# by date x mean using the preprocessing that performed best in
# all dates


best.preprocessing

keys.training.bymeanxdate


#     this must be updated if full models are run again

#test_sets_bydateXmean
selected.preprocessing <- list(
  "N.msc.arithmetic" = training.sets.columns$byDateXmean$training_columns$N$msc,
  "N.vi.arithmetic" = training.sets.columns$byDateXmean$training_columns$N$vi,
  "P.msc.arithmetic" = training.sets.columns$byDateXmean$training_columns$P$msc[c(1, 4, 7)],
  "P.vi.arithemetic" = training.sets.columns$byDateXmean$training_columns$P$vi[c(1, 4, 7)]
)
test_sets <- list(
  "N.msc.arithmetic" = test_sets_bydateXmean$N$msc,
  "N.vi.arithmetic" = test_sets_bydateXmean$N$vi,
  "P.msc.arithmetic" = test_sets_bydateXmean$P$msc[c(1, 4, 7)],
  "P.vi.arithmetic" = test_sets_bydateXmean$P$vi[c(1, 4, 7)]
)

#same applies here as above
job({
  trained.models.pls.by.date <-
    lapply(selected.preprocessing, function(list2) {
      lapply(list2, PLS.train)
    })
  saveRDS(trained.models.pls.by.date, "trained.models.pls.by.date")
}, import = "auto")

#trained.models.pls.by.date <- readRDS("trained.models.pls.by.date")
trained.models.pls.by.date %>% get_elements("results") %>% bind_rows(.id = "Model") %>%
  mutate(
    Nutrient = ifelse(Model %in% 1:6, "N", "P"),
    Preprocess = ifelse(Model %in% c(1:3, 7:9), "MSC", "SVI"),
    Month = ifelse(Model %in% c(seq(1, 10, 3)), "May",
                   ifelse(Model %in% c(seq(
                     2, 11, 3
                   )),
                   "June", "July"))
  ) %>%
  relocate(Nutrient, Preprocess, Month) %>%
  group_by(Nutrient, Preprocess, Month) %>%
  slice_min(RMSE) %>%
  arrange(Model %>% as.numeric()) %>%
  select(-Model) -> CV.by.date

CV.by.date %>% mutate(across(RMSE:MAESD, \(x) round(x, 2))) -> CV.by.date
library(clipr)
write_clip(CV.by.date)

## predictions by date =============================

### Nitrogen =========================================
N.test <- function(model, test.set) {
  data <- test.set
  y_pred <- predict(model, newdata = data)
  data <-
    data %>%  mutate(Predicted = y_pred) %>% relocate(Predicted)
  
  metrics.N <-
    data.frame(RMSE = caret::RMSE(y_pred, data$N),
               Rsquare = caret::R2(y_pred, data$N))
  
  return(list(
    "metrics" = metrics.N,
    "data" = data %>% select(Predicted, N, Treatment, TreeID)
  ))
  # return(metrics.N)
}


N.msc <-
  mapply(N.test,
         trained.models.pls.by.date$N.msc.arithmetic,
         test_sets$N.msc.arithmetic)

N.vi  <-
  mapply(N.test,
         trained.models.pls.by.date$N.vi.arithmetic,
         test_sets$N.vi.arithmetic)

### Phosphorus ========================================
P.test <- function(model, test.set) {
  data <- test.set
  # data = test_sets$P.msc.arithmetic[[3]]
  # model = trained.models.pls.by.date$P.msc.arithmetic[[3]]
  y_pred <- predict(model, newdata = data)
  data <-
    data %>%  mutate(Predicted = y_pred) %>% relocate(Predicted)
  
  # print(plot)
  metrics.P <-
    data.frame(
      RMSE = caret::RMSE(y_pred, data$P, na.rm = T),
      Rsquare = caret::R2(y_pred, data$P, na.rm = T)
    )
  
  return(list(
    "metrics" = metrics.P,
    "data" = data %>% select(Predicted, P, Treatment, TreeID)
  ))
  # return(metrics.P)
}

P.msc.arithmetic <-
  mapply(P.test,
         trained.models.pls.by.date$P.msc.arithmetic,
         test_sets$P.msc.arithmetic)

P.msc.arithmetic[1, 3]
P.vi.arithmetic <-
  mapply(P.test,
         trained.models.pls.by.date$P.vi.arithemetic,
         test_sets$P.vi.arithmetic)

bind_rows(P.msc.arithmetic["data", ], .id = "Month") -> P.msc.data
bind_rows(P.vi.arithmetic["data", ], .id = "Month") -> P.vi.data
bind_rows(N.msc["data", ], .id = "Month") -> N.msc.data
bind_rows(N.vi["data", ], .id = "Month") -> N.vi.data
bind_rows(
  list(
    "P:MSC" = P.msc.data,
    "P:SVI" = P.vi.data,
    "N:MSC" = N.msc.data,
    "N:SVI" = N.vi.data
  ),
  .id = "Model"
) %>%
  mutate(Month = fct_recode(
    Month,
    May = "1",
    June = "2",
    July = "3"
  )) %>%
  pivot_longer(c(P, N), names_to = "Nutrient", values_to = "Measured") %>% na.omit() ->
  plotting.data

library(ggh4x)

###plot results ===================================
#get p.values
library(stats)
library(broom)
plotting.data %>%
  group_by(Model, Month) %>%
  do(lmer = lmer(Measured ~ Predicted + (1 | Treatment), data = .)) %>%
  mutate(Tidy_Summary = list(tidy(lmer))) %>%
  unnest(Tidy_Summary) %>%
  filter(term == "Predicted") %>%
  mutate(p.adj = p.adjust(p.value, "holm")) %>%
  mutate(
    p.adj = signif(p.adj, 3),         # Round p.adj to 3 significant figures
    p.format = formatC(p.adj, format = "e", digits = 2), # Express p.adj in scientific notation
    sig = ifelse(p.adj < .001, "*", NA)) %>%  # Use scientific notation for p.format 
  # mutate(p.format = sub("e","%.% 10^", p.adj.sci)) %>% 
  separate(Model, into = c("Nutrient", "Preprocess"),
           remove = F)-> lm.by.month

plotting.data %>% filter(Model == "P:SVI", Month == "July") %>% 
 lmer(Measured ~ Predicted + (1| Treatment), data = .) %>% summary()

lm.by.month 
#BM
#fixed slope
(plotting.data %>% group_by(Nutrient) %>%
  do(
    plots =   ggplot(., aes(x = Predicted,  y = Measured)) +
      geom_point(aes(shape = Treatment, colour = Treatment)) +
      facet_wrap_equal(ncol = 3,
                       Model ~ Month) +
      theme_classic() +
      scale_colour(
        limits = c("cont", "APP", "N", "LN", "P", "MoP"),
        labels = c("Control", "APP", "-N" , "Low N", "-P","Low P")
      ) +
      scale_shape_discrete(
        limits = c("cont", "APP", "N", "LN", "P", "MoP"),
        labels = c("Control", "APP", "-N" , "Low N", "-P","Low P")
      ) +
      geom_abline() +
      #stat_cor() +  #uncomment for statistics
      #stat_regline_equation(vjust = 2, aes(label =  ..adj.rr.label..)) +
      xlab("Predicted Nutrient Concentration (%)") +
      ylab("Measured Nutrient Concentration (%)")) ->
  testsbydate.plot2)

(testsbydate.plot2$plots %>% ggarrange(plotlist = .,
                                      common.legend = T,
                                      nrow = 2) -> testsbydate.plot2)

lm.by.month %>% select(Model:Month, p.format) %>% group_by(Nutrient) %>% 
  select(Model, Month, Nutrient, p.format) %>% ungroup %>% 
  nest(annotations = c(Model, Month, p.format), .by = Nutrient)-> p.values 
{
( plotting.data %>% 
   group_by(Nutrient) %>%
   do(
     plots = ggplot(., aes(x = Predicted, y = Measured)) +
       geom_point(aes(shape = Treatment, colour = Treatment)) +
       facet_wrap_equal(ncol = 3, vars(Model, Month)) +
       theme_classic() +
       scale_colour(
         limits = c("cont", "APP", "N", "LN", "P", "MoP"),
         labels = c("Control", "APP", "-N" , "Low N", "-P","Low P")
       ) +
       scale_shape_discrete(
         limits = c("cont", "APP", "N", "LN", "P", "MoP"),
         labels = c("Control", "APP", "-N" , "Low N", "-P","Low P")
       ) +
       geom_abline() +
       xlab("Predicted Nutrient Concentration (%)") +
       ylab("Measured Nutrient Concentration (%)") 
   ) -> testsbydate.plots2)

testsbydate.plots2 %>% left_join(p.values, by = "Nutrient") %>% 
  mutate(plots = list(
    plots +
      geom_text(
        data = annotations,
        aes(x = -Inf, y = -Inf, label = paste("italic(p) == ", p.format)),
        vjust = -.4,
        hjust = -1.2,
        parse = T,
        inherit.aes = T
      ))) ->  testsbydate.plot2

 (ggarrange(plotlist = testsbydate.plot2$plots, nrow = 2,
           common.legend = T) -> testsbydate.plot2)
}
 
#summary
library(dplyr)
N.msc.metrics <- bind_rows(N.msc[1, ])
N.vi.metrics <- bind_rows(N.vi[1, ])
N.metrics <-
  bind_rows(N.msc.metrics, N.vi.metrics, .id = "id") %>%
  mutate(Nutrient = "N") %>%
  mutate(id = as.factor(id)) %>%
  mutate(id = plyr::revalue(id, c("1" = "msc", "2" = "vi"))) %>%
  mutate(Date = rep(keys.training.bymeanxdate$N$msc$Date, 2))

P.msc.metrics <- bind_rows(P.msc.arithmetic[1, ])
P.vi.metrics <- bind_rows(P.vi.arithmetic[1, ])
P.metrics <-
  bind_rows(P.msc.metrics, P.vi.metrics, .id = "id") %>%
  mutate(Nutrient = "P") %>%
  mutate(id = as.factor(id)) %>%
  mutate(id = plyr::revalue(id, c("1" = "msc", "2" = "vi"))) %>%
  mutate(Date = rep(unique(keys.training.bymeanxdate$P$msc$Date), 2))

Metrics <- bind_rows(N.metrics,
                     P.metrics) %>% dplyr::rename(Preprocess = id) %>%
  relocate(Nutrient)

#BM

library(tidyverse)
library(ggpubr)

Metrics %>% mutate(Not.trustworthy = ifelse(Nutrient == "P" &
                                              Date == "2023-05-02", Rsquare,  0)) ->
  Metrics #add column to allow exclusion of untrustworthy data
(ggplot(data = Metrics) +
  geom_bar(aes(x = Date,
               y = Rsquare,
               fill = Preprocess),
           stat = "identity",
           position = "dodge") +
  geom_bar(
    aes(x = Date, y = Not.trustworthy, group = Preprocess),
    alpha = 0.8,
    fill = "grey",
    stat = "identity",
    position = position_dodge()
  ) +
  facet_wrap( ~ Nutrient, nrow = 2) +
  theme_classic() +
  scale_fill(limits = c("msc", "vi"),
             labels = c("MSC", "SVI")) +
  ylab("R-squared (Test)") +
  theme(
    text = element_text(size = 12),
    legend.position = c(0.27, 0.62),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.box.background = element_rect(colour = "black", linewidth = 1)
  ) -> RMSE.Date.Comparison)

#metrics
(Metrics %>% relocate(Nutrient, Date) %>%
    mutate(Preprocess = str_replace_all(Preprocess, 
                                        pattern = c("msc" = "MSC", "vi" = "SVI"))) %>% 
    mutate(Month = month(Date, label = T, abbr = F) %>%
             as.character()) %>% 
    left_join(lm.by.month, by = c("Nutrient", "Preprocess", "Month")) %>%
    select(-c(Not.trustworthy, Month:sig)) %>% 
    mutate(across(RMSE:Rsquare, round, 2))-> x)
write_clip(x)


#using facet_wrap_equal
testsbydate.plot2 +
  theme(legend.position = "none",
        axis.text = element_text(size = 8)) -> x
RMSE.Date.Comparison -> y
#theme(legend.position = "none") -> y
ggarrange(x, y, labels = "auto", widths = c(4.5, 2)) 

#       Important Wavelengths      ============================================
trained.models.pls.by.date %>% str(2)
best.models.all.dates %>% str(2)

library(plsVarSel)
bestTunes <-
  best.models.all.dates %>% get_elements("bestTune") %>% unlist()
finalModels <-
  best.models.all.dates %>% get_elements("finalModel")
trainingDatas <-
  best.models.all.dates %>% get_elements("trainingData") %>% lapply(function(x) {
    x[".outcome"] <- NULL
    x
  })

mapply(VIP, finalModels, bestTunes) %>%
  lapply(as_tibble, rownames = "Wavelength") %>%
  bind_rows(.id = "id")  %>%
  separate(id, c("Nutrient", "Correction")) %>%
  group_by(Nutrient, Correction) %>%
  dplyr::rename(VIP = value) %>%
  filter(VIP > 1) %>%
  mutate(Wavelength = gsub("`", "", Wavelength)) -> important.wavelengths.all.dates

(ggplot(
  important.wavelengths.all.dates %>% filter(Correction == "msc"),
  aes(x = as.numeric(Wavelength), y = VIP)
) +
  geom_histogram(stat = "identity") +
  facet_wrap( ~ Nutrient) +
  theme_classic() +
  xlab("Wavelength (nm)") -> VIP.plot
)

important.wavelengths.all.dates %>% 
  filter(Nutrient == "N", Correction == "msc") %>%
  arrange(desc(VIP)) #%>% View()

important.wavelengths.all.dates %>% 
  filter(Nutrient == "P", Correction == "msc") %>%
  arrange(as.numeric(Wavelength))

imp.SVI.VIP <-
  important.wavelengths.all.dates %>% filter(Correction == "vi") %>% arrange(desc(VIP))

(
  imp.msc <-
    important.wavelengths.all.dates %>% dplyr::filter(Correction == "msc") %>%
    mutate(Wavelength = as.numeric(Wavelength))
)

mRMR(finalModels$P.vi.arithmetic,
     trainingDatas$P.vi.arithmetic,
     nsel = 30)

#importance by mRMR
mapply(mRMR,
       pls.object =  finalModels,
       X = trainingDatas ,
       nsel = 30)[1, ] %>%
  lapply(as_tibble, rownames = "Wavelength") %>%  bind_rows(.id = "id")  %>%
  separate(id, c("Nutrient", "Correction")) %>%
  group_by(Nutrient, Correction) %>%  dplyr::rename(mRMR = value) %>%
  mutate(Wavelength = gsub("X", "", Wavelength)) -> imp.all.dates.mRMR


imp.all.dates.mRMR %>% filter(Correction == "vi") %>%
  group_by(Nutrient) %>% mutate(rank = row_number()) ->
  imp.SVI.mRMR

imp.SVI.VIP %>% ungroup() %>%
  full_join(imp.SVI.mRMR, by = c("Nutrient", "Wavelength")) %>%
  mutate(mRMR.rank = rank) %>%
  select(-c(Correction.x, Correction.y, mRMR, rank)) %>%
  arrange(Nutrient, desc(VIP)) -> imp.SVI.VIP.mRMR

write_clip(imp.SVI.VIP.mRMR)

imp.SVI.VIP.mRMR %>% group_by(Nutrient) %>%  arrange(mRMR.rank) %>% 
  slice_min(mRMR.rank, n = 13) # %>% View()
#TODO edit and add to document


ggplot.ribbon <- function(long.dataset, alpha = .1) 
  {
  long.dataset <- data.to.plot
  long.dataset %>%
    group_by(Wavelength, Treatment, Nutrient) %>%
    dplyr::summarise(Mean = mean(Reflectance), SD = sd(Reflectance)) %>%
    mutate(Wavelength = as.numeric(Wavelength)) ->
    long.dataset
  
  ggplot(data = long.dataset) +
    geom_ribbon(aes(
      x = Wavelength,
      y = Mean,
      ymin = Mean - SD,
      ymax = Mean + SD,
      fill = Treatment
    ),
    alpha = alpha) +
    geom_line(
      aes(x = Wavelength,
          y = Mean,
          colour = Treatment),
      #size = .8)
      )+
      theme_classic() +
        facet_wrap( ~ Nutrient)
}
bind_rows(
  N =  data_sets$N$data_sets$msc,
  P = data_sets$P$data_sets$msc,
  .id = "Nutrient"
) %>%
  filter(Mean == "arithmetic")  %>% pivot_longer(`365`:`2500`, names_to = "Wavelength", values_to = "Reflectance") %>%
  filter((Nutrient == "N"  &
            Treatment %in% c("cont", "N", "LN")) |
           (Nutrient == "P"  &
              Treatment %in% c("cont", "P", "MoP"))) -> data.to.plot

(ggplot.ribbon(data.to.plot, alpha = 0 ) +
  geom_vline(
    data = imp.msc,
    aes(xintercept = Wavelength),
    colour = "lightgrey" ,
    alpha = .5,
    show.legend = F
  ) +
  geom_vline(
    data = imp.all.dates.mRMR %>% filter(Correction == "msc"),
    aes(xintercept = Wavelength %>% as.numeric()),
    alpha = 0.5,
    colour = "orange" ,
    show.legend = F
  ) +
  geom_line(aes(x = Wavelength,
                y = Mean,
                colour = Treatment),
            linewidth = .8) +
  scale_fill(
    labels = c("Control", "APP", "-N" , "Low N", "-P", "Low P"),
    limits = c("cont", "APP", "N", "LN", "P", "MoP")
  ) +
  scale_colour(
    labels = c("Control", "APP", "-N" , "Low N", "-P", "Low P"),
    limits = c("cont", "APP", "N", "LN", "P", "MoP")
  ) +
  scale_alpha(range = c(0, 5)) +
  ylab("MSC-Reflectance") +
  xlab("Wavelegnth (nm)") +
  theme(text = element_text(size = 12)) +
  facet_wrap(~Nutrient, nrow = 2) -> variable.importance)

#  Backward Feature Elimination  =============================================

detach("package:hsdar", unload = T)

#adjust PLS train function to work better with RFE
PLS.train <- function(dataset) {
  #dataset = data
  dataset <- as.data.frame(dataset)
  ctrl <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 3,
    verboseIter = TRUE
  )
  
  PLS <- caret::train(
    y = dataset[, 1],
    x = dataset[, -1, drop = F],
    method = 'pls',
    tuneLength = 20,
    verboseIter = T,
    preProcess = c("center", "scale"),
    trControl = ctrl
  )
}

# Get importance using mRMR algorithm and data to plot:
# rerunning will produce slightly different results each time and take 
# condiserable time (approximately 20 minutes). 
# The results can be loaded from a saved object:
job({
  remove(model)
  lapply(best.models.all.dates, function(model) {
    #model = best.models.all.dates$N.msc
    nsel <-
      c(2:30, seq(40, 100, 10), 115, seq(100, dim(model$trainingData)[2] -
                                           1, 50)) %>%
      unique()
    lapply(nsel %>% as.list, function(x) {
      #x = 2
      model$finalModel %>% mRMR(nsel = x, model$trainingData %>%
                                  select(-.outcome)) -> variables
      model$trainingData %>% dplyr::select(.outcome, variables$selection) ->
        data
      PLS.train(data) -> model
      model$results %>% slice_min(RMSE)
    }) -> results
    results %>% bind_rows() %>% mutate(nsel = nsel)
  }) -> learning.curve.data.mRMR
}, import = "auto")



# Get importance using VIP algorithm and data to plot
job({
  lapply(best.models.all.dates, function(model) {
    #model = best.models.all.dates$N.msc
    nsel <-
      c(2:30, seq(40, 100, 10), 115, seq(100, dim(model$trainingData)[2] -
                                           1, 50)) %>%
      unique()
    lapply(nsel %>% as.list, function(x) {
      # x = 10
      model$finalModel %>% VIP(opt.comp = model$bestTune[1, 1]) %>%
        as_tibble(rownames = "Wavelength") %>%
        rename(VIP = value) %>%
        arrange(desc(VIP)) %>%
        mutate(Wavelength = gsub("`", "", Wavelength)) %>%
        slice_max(VIP, n = x) %>%
        dplyr::select(Wavelength) -> variables
      model$trainingData %>% dplyr::select(.outcome, variables[[1]]) -> data
      PLS.train(data) -> model
      model$results %>% slice_min(RMSE)
    }) -> results
    results %>% bind_rows() %>% mutate(nsel = nsel)
  }) -> learning.curve.data.VIP
}, import = "auto")

#BM
RFE.results <- list(mRMR = learning.curve.data.mRMR,
                    VIP = learning.curve.data.VIP)
#saveRDS(RFE.results, "RFE.results")

#load saved data files 
#RFE.results <- readRDS("RFE.results")
learning.curve.data.mRMR <- RFE.results[["mRMR"]]
learning.curve.data.VIP <- RFE.results[["VIP"]]
# Plot Results
learning.curve.data.mRMR %>% bind_rows(.id = "id") %>%
  separate(id, into = c("Nutrient", "Preprocess")) -> data.to.plot.mRMR

learning.curve.data.VIP %>% bind_rows(.id = "id") %>%
  separate(id, into = c("Nutrient", "Preprocess")) -> data.to.plot.VIP

bind_rows(mRMR = data.to.plot.mRMR, VIP = data.to.plot.VIP, .id = "Sel_proc") ->
  data.to.plot

(ggplot(data.to.plot, aes(x = nsel)) +
  geom_ribbon(aes(
    ymin = RMSE - RMSESD,
    ymax = RMSE + RMSESD,
    fill = Sel_proc
  ),
  alpha = 0.3,
  ) +
  geom_line(aes(y = RMSE, lty = Sel_proc, colour = Sel_proc), linewidth = .8) +
  facet_grid2(
    Preprocess ~ Nutrient,
    scales = "free",
    labeller = labeller(Preprocess = c("msc" = "MSC", "vi" = "SVI")),
    independent = "all"
  ) +
  theme_classic() +
  scale_fill_manual(name = "", values = c("Orange", "grey")) +
  scale_colour_manual(name = "", values = c("Orange", "grey")) +
  scale_linetype_discrete(name = "") +
  xlab("Number of Selected Features") +
  ylab("RMSE (Cross-Validated)") +
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  xlim(c(0, 100)) -> rfe.plot)


text <- theme(text = element_text(size = 10))
ggarrange(
  variable.importance + text,
  rfe.plot + text,
  nrow = 2,
  heights = c(1, 2.5),
  labels = "auto",
  align = "hv"
)



