



#   Foil Tunnel Leaf     #
# nutrient analysis 2023 #

#Document set up ================

# make sure all data are unzipped (Raw_Data.zip, training_trees.zip, test_trees.zip)
#general libraries
library(tidyverse)

#set working directory: all paths are relative to the script location. 
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
  dir_ls("../Raw_Data/Analysis Results/Phosphorus") %>% as.list()
nutrient.content.files.N <-
  dir_ls("../Raw_Data/Analysis Results/Nitrogen") %>% as.list()

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
  list.files("../Raw_Data/Analysis Results/Phosphorus/") %>%
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
  list.files("../Raw_Data/Analysis Results/Nitrogen/") %>%
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

#visualise nutrient data 
#create function to identify outliers in the nutrient concentration data. 
# identified as t interquartile ranges more or less than the upper and lower 
#quartiles, respectively
is_outlier <- function(x, t = 3) {
  return(x < quantile(x, 0.25) - t * IQR(x) |
           x > quantile(x, 0.75) + t * IQR(x))
}

# data wrangling for visualisation
Long.all.dates <-
  Long.all.dates %>% group_by(Nutrient, Date) %>%
  mutate(outlier = ifelse(
    is_outlier(Concentration),
    as.numeric(Concentration),
    as.numeric(NA)
  ))

library(ggsci)

plot.nutrients.Date.x <- function(data) {
  ggplot(data = data %>% 
           #comment out to include outliers
           filter(is.na(outlier)), aes(x = as.factor(Date), y = Concentration)) +
    geom_boxplot(aes(fill = Treatment),
                 alpha = 0.4,
                 outlier.size = -1) +
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


(boxplot <-
    plot.nutrients.Date.x(
      data = dplyr::filter(Long.all.dates, Nutrient %in% c("N", "P")) %>%
        mutate(Treatment = factor(
          Treatment,
          levels = c("cont", "APP", "N", "LN", "P", "MoP"),
          ordered = T
        ))
    ))

#remove outliers
Long.all.dates %>% filter(is.na(outlier))
Long.all.dates %>% filter(is.na(outlier)) -> Long.all.dates #remove outliers

data = Long.all.dates

#work with only N and P data from here on and more data wrangling
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
#ANOVA of Foliar N and P
library(ggpubr)
compare_means(
  Concentration ~ Treatment,
  group.by = c("Nutrient", "Date"),
  method = "anova",
  data = data
)

#data wrangling for visualisation
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

# add significance to boxplot
(boxplot +
    geom_text(
      data = means %>% 
        mutate(p.signif = str_replace(p.signif, "ns", "")) %>%
        replace_na(list(p.signif = "")),
      aes(
        y = UQ,
        x = as.factor(Date),
        label = p.signif,
        group = Treatment
      ),
      #position = position_jitterdodge(dodge.width = 0.6, jitter.width = 0, jitter.height = 1),
      position = position_dodge(width = .75),
      size = 3.5,
      hjust = -.07,
      vjust = 0.35
    ) +
    theme(text = element_text(size = 12),
          legend.position = "none") -> boxplot)

#ggsave("Figure_1.pdf", boxplot)


# check assumptions
library(car)

data  %>%
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

#Keeping tree information for sd
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

data <- TotalShootGrowth
# means of shoot growth for labels
UQs <-  data %>% group_by(Treatment) %>%
  summarise(UQ = quantile(TotalShootGrowth, .75),
            IQR = IQR(TotalShootGrowth)) %>%  
  mutate(Treatment = factor(
    Treatment,
    levels = c("C", "APP", "-N", "Low N", "-P", "Low P"),
    ordered = T
  ))

data %>% ungroup %>% distinct(Treatment) %>%  mutate(group2 = "C", p.signif = NA) %>% 
  filter(Treatment == "C") -> controls
data %>% mutate(Treatment = factor(Treatment, ordered = F)) %>%
  compare_means(
    formula = TotalShootGrowth ~ Treatment,
    data = .,
    method = "t.test",
    ref.group = "C"
  ) %>% 
  bind_rows(controls) %>%  mutate(Treatment = factor(
    group2,
    levels = c("C", "APP", "-N", "Low N", "-P", "Low P"),
    ordered = T
  )) %>% 
  arrange(Treatment) %>% left_join(UQs, by = c("Treatment")) -> means

#shoot growth boxplot
(ggplot(data = TotalShootGrowth, aes(x = Treatment, y = log(TotalShootGrowth *
                                                              1000))) +
    geom_boxplot(aes(fill = Treatment),
                 alpha = 0.4,
                 outliers = F) +
    # geom_point(aes(colour = Treatment),
    #            position = position_jitter(width = 0.2),
    #            size = 2) +
    theme_classic() +
    #stat_compare_means(method = "anova", label.y.npc = "bottom", label.x.npc = "left", label = "p.format")+
    geom_text(
      data = means %>% 
        mutate(p.signif = str_replace(p.signif, "ns", "")) %>%
        replace_na(list(p.signif = "")),
      aes(
        y = log(UQ*1000),
        label = p.signif,
        group = Treatment
      ),
      size = 3.5,
      hjust = -.07,
      vjust = .35
    )+
    scale_fill() +
    scale_colour() +
    ylab("Log(Total Shoot Growth (mm))") +
    theme(text = element_text(size = 12)) -> shoot.growth.boxplot)

#combine with nutrient concentrations into one plot
(
  nutrient.boxplots <-
    ggpubr::ggarrange(
      shoot.growth.boxplot,
      boxplot,
      nrow = 1,
      legend = "right",
      labels = "auto",
      widths = c(2, 4),
      common.legend = T
    )
)

#save to file (commented out so as not to overight existing files)
# scalar = 1.5
# ggsave("Figure_1.pdf", nutrient.boxplots,
#        width = 6*scalar,
#        height = 5*scalar)

##Spectral Data ======================
#                      All of this is data wrangling
# Combine Nutrient data and mean spectra according to the labels we
# took in the field (i.e. which scan belongs to each tree). You can run this 
# code once and, thereafter, just load the final data set at the end.

#load spectral data
library(spectrolab)

# sed files must be saved directly in each months folder (no subfolders)
setwd(dirname(getActiveDocumentContext()$path))
setwd("../Raw_Data/SED files/")
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
metadata %>% get_elements("dims") %>% bind_rows(.id = "Date") %>%
  select(Date, n_samples) %>%
  adorn_totals()

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

#import.labels: NB! must have one file for each date. All files should have the date
# as the file name in the form 2023_May_30

label.files <- dir_ls("../Raw_Data/Excel trees/") %>%
  `names<-`(names(df.by.date)) #NB! the order of the data frames must be exactly the same as label files
label.files %>% names()

#colnames of each file must all be the the same:
#Scan, Nutrient, TreeID, CIF (although not
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

#Merge labels and scans and calculate
#tree/leaf means for each nutrient

#calculate mode (useful for summarising string columns)
calculate_mode <-
  function(x) {
    # function for aggregating tree treatment class
    uniqx <- unique(na.omit(x))
    uniqx[which.max(tabulate(match(x, uniqx)))]
  }


#merge labels and scans (means
#spectra for P estimation are also done here)
# given two data frames, one containing spectra (as rows) and the other
# metadata, merge the two according to their identifier (Scan)
merge.labels.and.scans <- function(labels, scans) {
  labels$Scan <- as.character(labels$Scan)
  scans$Scan <- as.character(scans$Scan)
  
  # for P where spectra need to be averaged, arithmetic, geometric and harmonic
  # means are calculated. In each case, the mean of all spectra taken from the
  # same tree (with the same TreeID) whose leaves were submitted for analysis of
  # the same foliar nurient are is calculated.
  df.arithmetic.mean <- inner_join(labels, scans, by = "Scan") %>%
    group_by(TreeID, Nutrient) %>%
    summarise(across(Scan:Channels, calculate_mode),
              across(`350`:`2500`, mean))
  
  #harmonic mean
  library(psych)
  df.harmonic.mean <- inner_join(labels, scans, by = "Scan") %>%
    group_by(TreeID, Nutrient) %>%
    summarise(across(Scan:Channels, calculate_mode),
              across(`350`:`2500`, harmonic.mean))
  # geometric mean
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
  # classify all
  df$Treatment <- gsub("extra", "cont", df$Treatment)
}

#apply function
df.by.date <-
  mapply(merge.labels.and.scans, labels.by.date, df.by.date)

#extract and clean output
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


##N contains just N and P contains all other nutrients. Therefore we must work with different data frames for N and P or with an impossibly long dataset
df.by.nutrient <- df.all %>% group_by(Nutrient) %>% group_split()
df.all %>% group_by(Nutrient) %>% group_keys()

#add experimental information, merge by date and tree ID.
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
  select(Nutrient, Date, Date1) %>% distinct() # check that dates are the same
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


#once the code above is run you can just load the saved object
df.list <- readRDS("dfs_GH_Nutrients_2023")
# df.list <- list("N" = df.N, "P" = df.P) # list of dataframes for
# N and P: P has all other nutrients as well and thus both cannot be
#in the same dataframe.
# detach("package:spectrolab", unload = T)


# Process data for spectral for analysis     ====================================


#The data analysis presented in the research article was performed using hsdar.
# However, hsdar has been removed from CRAN. The package is supplied in the
# gitub repository which can be copied and pasted into your directory that
# R accesses libraries. This analysis can be performed using the alternative 
# script R_script_hsdar.R. It does however have rgdal as a dependency which 
# can no longer run on R.4.5. What follows is a work around that does not use the 
# functions provided in hsdar. 

library(pls)

# recode extra trees as controls
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

##visualisations========================

#Plotting function

# functions to easily plot speclibs using ggplot

plot.spectra.df.mean <- function(spectra.to.plot,
                                 colour_by,
                                 range_start = `365`, 
                                 range_end = `2500`, 
                                 facet = NULL,
                                 facet2 = NULL,
                                 linewidth = 1.2,
                                 alpha = 0.2,
                                 show_NA = T) {
  dfe  <-
    pivot_longer(spectra.to.plot, {{range_start}}:{{range_end}},
                 names_to = "Wavelength", values_to = "Reflectance") %>% 
    group_by(Wavelength, {{facet}}, {{facet2}}, {{colour_by}}) %>% 
    relocate(Wavelength, {{colour_by}}, {{facet}}, {{facet2}},Reflectance) %>% 
    summarise( SD = sd(Reflectance, na.rm = T), Reflectance = mean(Reflectance, na.rm = T)) %>% 
    mutate(Wavelength = as.numeric(as.character(Wavelength))) %>% arrange(Wavelength)
  
  if(show_NA == T) {
    ggplot(data =dfe %>%  mutate({{colour_by}} := {{colour_by}} %>% replace_na("Unknown")))+
      geom_ribbon(aes(x = Wavelength,
                      y = Reflectance, 
                      ymin = Reflectance - SD,
                      ymax = Reflectance + SD,
                      fill = {{colour_by}}), 
                  alpha = alpha) +
      geom_line(aes(x = Wavelength,
                    y = Reflectance,
                    colour = {{colour_by}}),
                linewidth = linewidth
      )+
      theme_classic()+
      scale_fill_jco()+
      scale_colour_jco()+
      xlab("Wavelength (nm)")
    
  } else {
    ggplot(data =dfe %>% filter(!is.na({{colour_by}})))+
      geom_ribbon(aes(x = Wavelength,
                      y = Reflectance, 
                      ymin = Reflectance - SD,
                      ymax = Reflectance + SD,
                      fill = {{colour_by}}), 
                  alpha = alpha) +
      geom_line(aes(x = Wavelength,
                    y = Reflectance,
                    colour = {{colour_by}}),
                linewidth = linewidth
      )+
      theme_classic()+
      scale_fill_jco()+
      scale_colour_jco()+
      xlab("Wavelength (nm)")
  }
  
  #scale_colour_manual(values = pal)
  #scale_color_viridis(option = "D")
}

#Plot spectra per treatment for N and P spectra
df.list[[1]] %>% rename(Concentration = N) -> df.N
df.list[[2]] %>% rename(Concentration = P) %>% select(-c(K:Si, Cu)) -> df.P
bind_rows(df.N, df.P) -> df

# Using the full data set requires substantial RAM. By unccommenting below 
# a subset of the data set can be sampled which is useful for trouble shooting
df %>% dim()
# df %>% group_by(Nutrient, Treatment, Date, Mean) %>% slice_sample(n = 10) -> df
remove(df.N, df.P) 
gc()

df %>% plot.spectra.df.mean(colour_by = Treatment, facet = Nutrient, facet2 = Date)+
  facet_grid(Nutrient~Date)

#outlier removal by spectral angle ======================================

#calculate spectral angle against mean spectra

#spectral angle is the arc cos of the dot product, over norms
#function to calculate spectral angle to means of each group of a data frame 

df %>% group_by(Nutrient) %>% rownames_to_column("SpectraID") %>% 
  nest(spectra = `350`:`2500`) -> df

spec_angle <- function(spectrum.df.group, mean.spectrum) {
  # spectrum.df.group <- df %>% filter(Nutrient == "N") %>% unnest(spectra)
  # mean.spectrum = mean.spectra[[1]][1,]
  spectrum.df.group %>% column_to_rownames("SpectraID") %>% 
    select(`350`:`2500`) %>% as.matrix() -> spec.mat
  mean.spectrum[1,] %>% as.numeric -> mean.spectrum
  
  spec.mat %*% mean.spectrum -> dot.product
  spec.mat %>% apply(1, \(x) sqrt(sum(x^2))) -> spec.norms
  sqrt(sum(mean.spectrum^2)) -> mean.norm 
  spec_angle <- acos(dot.product/(spec.norms*mean.norm)) %>% 
    enframe(name = "SpectraID", value = "Spec_angle")
  return(spec_angle)
}

#mean spectra
df %>% unnest(spectra) %>% 
  group_by(Nutrient) %>% 
  summarise(across(`350`:`2500`, mean, na.rm = T)) -> mean.spectra

#calculate spectral angles
spec_angle(df %>% filter(Nutrient == "N") %>% unnest(spectra), 
           mean.spectra[-2,-1]) -> spec_angle.N

spec_angle(df %>% filter(Nutrient == "P") %>% unnest(spectra), 
           mean.spectra[-1,-1]) -> spec_angle.P

#bind together
bind_rows(spec_angle.P, spec_angle.N) %>% 
  mutate(SpectraID = SpectraID %>% as.numeric()) %>% 
  arrange(SpectraID) -> spec_angles

remove(spec_angle.N, spec_angle.P)
gc()

# merge 
df %>% mutate(SpectraID = SpectraID %>% as.numeric) %>% 
  left_join(spec_angles, "SpectraID") %>% 
  ungroup %>%   
  mutate(Spec_ang_std = scale(Spec_angle)) -> a 

a %>% select(SpectraID, Spec_angle, Spec_ang_std) %>% 
  ggplot(aes(x = Spec_ang_std))+
  geom_histogram()

a %>% filter(abs(Spec_ang_std) > 3) %>% unnest(spectra) %>% 
  mutate(SpectraID = as.factor(SpectraID)) -> b

b %>% pivot_longer(`350`:`2500`, names_to = "Wavelength", values_to = "Reflectance") %>% 
  mutate(across(c(Wavelength, Reflectance), as.numeric)) %>% 
  ggplot(aes(x = Wavelength, y = Reflectance, colour = SpectraID))+
  geom_line()

#remove outliers
a %>% filter(abs(Spec_ang_std) < 3) -> df

# #remove noisy bit in the beginning (first 15 bands)
df %>% unnest(spectra) %>% select(-c(`350`:`364`)) %>% 
  nest(spectra = `365`:`2500`) -> df

remove(a,b, data, Long.all.dates, Long.all.dates.N, Long.all.dates.P, df.list)
gc()
# preprocess spectra: =======================================
# MSC, first deriviative, second derivative and veg indices
library(signal)
filter <- dplyr::filter
select <- dplyr::select

## SVIs =========================================
#create set of veg indices: this was originally performed using hsdar which
#is no longer available. Now it is done with an adaption of the function 
#hsdar::vegindex()
vegindex <- function(df) {
  df_long <- df %>% select(SpectraID, spectra) %>% unnest(spectra) %>% 
    pivot_longer(`365`:`2500`, names_to = "Wavelength", values_to = "Reflectance") %>%
    mutate(Wavelength = as.numeric(Wavelength)) %>%
    arrange(SpectraID, Wavelength)
  
  # Apply Savitzky-Golay derivatives per sample
  D1 <- df_long %>%
    group_by(SpectraID) %>%
    mutate(
      D1 = sgolayfilt(Reflectance, m = 1)) %>% 
    pivot_wider(id_cols = SpectraID, 
                names_from = Wavelength, 
                values_from = D1)
  
  # Structural indices
  vis <- df %>% 
    unnest(spectra) %>% 
    transmute(
      SpectraID = SpectraID,
      NDVI = (`800`-`680`)/(`800`+`680`),
      OSAVI = (1+0.16)*(`800`-`670`)/(`800`+`670`+0.16),
      RDVI = (`800`-`670`)/sqrt(`800`+`670`),
      SAVI = (1+0.5)*(`800`-`670`)/(`800`+`670`+0.5),
      MTVI = 1.2*(1.2*(`800`-`550`)-2.5*(`670`-`550`)),
      
      # Water indices
      NDWI = (`860`-`1240`)/(`860`+`1240`),
      PWI = `900`/`970`,
      MSI = `1600`/`817`,
      WBI = `970`/`900`,
      SRWI = `850`/`1240`,
      
      # Chlorophyll and red edge indices
      GMI1 = `750`/`550`,
      GMI2 = `750`/`700`,
      MCARI = ((`700`-`670`)-0.2*(`700`-`550`))*(`700`/`670`),
      TVI = 0.5*(120*(`750`-`550`)-200*(`670`-`550`)),
      Vogelmann4 = (`734`-`747`)/(`715`+`720`),
      CARI = {
        a <- (`700`-`550`)/150
        b <- `550`-(a*550)
        `700`*abs(a*670+`670`+b)/(`670`*(a^2+1)^0.5)
      },
      CI = (`675`*`690`)/(`683`^2),
      Carter = `695`/`420`,
      Carter2 = `695`/`760`,
      Carter3 = `605`/`760`,
      Carter4 = `710`/`760`,
      Carter5 = `695`/`670`,
      Carter6 = `550`,
      Datt = (`850`-`710`)/(`850`-`680`),
      Datt2 = `850`/`710`,
      Datt4 = `672`/(`550`*`708`),
      Datt5 = `672`/`550`,
      Datt6 = `860`/(`550`*`708`),
      Datt7 = (`860`-`2218`)/(`860`-`1928`),
      Datt8 = (`860`-`1788`)/(`860`-`1928`),
      DD = (`749`-`720`)-(`701`-`672`),
      DDn = 2*(`710`-`660`-`760`),
      EVI = 2.5*((`800`-`670`)/(`800`-(6*`670`)-(7.5*`475`)+1)),
      GI = `554`/`677`,
      Gitelson = 1/`700`,
      Gitelson2 = (`750`-`800`)/(`695`-`740`)-1,
      `Green NDVI` = (`800`-`550`)/(`800`+`550`),
      `MCARI/OSAVI` = MCARI/OSAVI,
      MCARI2 = ((`750`-`705`)-0.2*(`750`-`550`))*(`750`/`705`),
      `MCARI2/OSAVI2` = MCARI2/((1+0.16)*(`750`-`705`)/(`750`+`705`+0.16)),
      mNDVI = (`800`-`680`)/(`800`+`680`-2*`445`),
      mND705 = (`750`-`705`)/(`750`+`705`-2*`445`),
      Maccioni = (`780`-`710`)/(`780`-`680`),
      MSAVI = 0.5*(2*`800`+1-((2*`800`+1)^2-8*(`800`-`670`))^0.5),
      mSR = (`800`-`445`)/(`680`-`445`),
      mSR705 = (`750`-`445`)/(`705`-`445`),
      mSR2 = (`750`/`705`)-1/((`750`/`705`+1)^0.5),
      MTCI = (`754`-`709`)/(`709`-`681`),
      NDVI2 = (`750`-`705`)/(`750`+`705`),
      NDVI3 = (`682`-`553`)/(`682`+`553`),
      NPCI = (`680`-`430`)/(`680`+`430`),
      OSAVI2 = (1+0.16)*(`750`-`705`)/(`750`+`705`+0.16),
      REP_Li = 700 + 40*(((`670`+`780`)/2 - `700`)/(`740`-`700`)),
      SIPI = (`800`-`445`)/(`800`-`680`),
      SPVI = 0.4*3.7*(`800`-`670`)-1.2*((`530`-`670`)^2)^0.5,
      SR = `800`/`680`,
      SR1 = `750`/`700`,
      SR2 = `752`/`690`,
      SR3 = `750`/`550`,
      SR4 = `700`/`670`,
      SR5 = `675`/`700`,
      SR6 = `750`/`710`,
      SR7 = `440`/`690`,
      SR8 = `515`/`550`,
      SRPI = `430`/`680`,
      TCARI = 3*((`700`-`670`)-0.2*(`700`-`550`)*(`700`/`670`)),
      TCARI2 = 3*((`750`-`705`)-0.2*(`750`-`550`)*(`750`/`705`)),
      `TCARI/OSAVI` = TCARI/OSAVI,
      `TCARI2/OSAVI2` = TCARI2/((1+0.16)*(`750`-`705`)/(`750`+`705`+0.16)),
      Vogelmann = `740`/`720`,
      Vogelmann2 = (`734`-`747`)/(`715`+`726`),
      
      # Other indices
      PRI = (`531`-`570`)/(`531`+`570`),
      PRI_norm = PRI*(-1)/(((`800`-`670`)/sqrt(`800`+`670`))*(`700`/`670`)),
      CAI = 0.5*(`2000`+`2200`)-`2100`,
      NDNI = (log(1/`1510`) - log(1/`1680`))/(log(1/`1510`) + log(1/`1680`)),
      NDLI = (log(1/`1754`) - log(1/`1680`))/(log(1/`1754`) + log(1/`1680`)),
      PARS = `746`/`513`,
      PSSR = `800`/`635`,
      PSND = (`800`-`470`)/(`800`+`470`),
      CRI1 = 1/`515`-1/`550`,
      CRI2 = 1/`515`-1/`700`,
      CRI3 = (1/`515`-1/`550`)*`770`,
      CRI4 = (1/`515`-1/`700`)*`770`,
      MPRI = (`515`-`530`)/(`515`+`530`),
      `PRI*CI2` = PRI*(`760`/`700`-1),
      CI2 = `760`/`700`-1,
      PSRI = (`678`-`500`)/`750`,
      TGI = -0.5*(190*(`670`-`550`)-120*(`670`-`480`)),
      GDVI_2 = (`800`^2-`680`^2)/(`800`^2+`680`^2),
      GDVI_3 = (`800`^3-`680`^3)/(`800`^3+`680`^3),
      GDVI_4 = (`800`^4-`680`^4)/(`800`^4+`680`^4),
      LWVI1 = (`1094`-`983`)/(`1094`+`983`),
      LWVI2 = (`1094`-`1205`)/(`1094`+`1205`),
      DWSI1 = `800`/`1660`,
      DWSI2 = `1660`/`550`,
      DWSI3 = `1660`/`680`,
      DWSI4 = `550`/`680`,
      DWSI5 = (`800`+`550`)/(`1660`+`680`),
      `SWIR FI` = (`2133`^2)/(`2225`*`2209`^3),
      `SWIR LI` = 3.87*(`2210`-`2090`)-27.51*(`2280`-`2090`)-0.2,
      `SWIR SI` = -41.59*(`2210`-`2090`)+1.24*(`2280`-`2090`)+0.64,
      `SWIR VI` = 37.72*(`2210`-`2090`)+26.27*(`2280`-`2090`)+0.57
    )
  
  # Derivative-based indices (using D1 data)
  d1vis <- D1 %>% transmute(
    SpectraID = SpectraID,
    Boochs = `703`,
    Boochs2 = `720`,
    Datt3 = `754`/`704`,
    D1 = `730`/`706`,
    D2 = `705`/`722`,
    EGFR = {
      # Find max in green region (500-550) and red edge region (650-750)
      max(`650`:`750`)/max(`500`:`550`)
    },
    EGFN = {
      # Similar to EGFR but normalized difference
      max(`650`:`750`)-max(`500`:`550`)/max(`650`:`750`)+max(`500`:`550`)
    },
    Vogelmann3 = `715`/`705`,
    Sum_Dr1 = {
      # Sum of absolute derivative values from 626-795 nm
      sum(abs(`626`:`785`))
    },
    Sum_Dr2 = {
      # Sum of derivative values from 680-780 nm
      sum(`680`:`780`)
    },
    mFR = (`680`-`700`)/(680-700),
    tFR = `680` - mFR * 680,
    mNIR = (`725`-`760`)/(725-760),
    tNIR = `725` - mNIR * 725,
    REP_LE = {
      # Linear extrapolation method for red edge position
      # This is a simplified version - the full implementation requires more complex linear fitting
      # Using approximation with available wavelengths
      (tNIR-tFR)/(mFR-mNIR)
    },
    DPI = (`688`*`710`)/(`697`^2),
    ClAInt = {
      # Integration from 600-735 nm
      sum(abs(`600`:`735`))
    }
  ) %>% select(-c(mFR:tNIR))
  
  # Combine regular and derivative indices
  result <- vis %>% 
    left_join(d1vis, by = "SpectraID")
  
  return(result)
}

SVIs <- vegindex(df) %>% select(where(~ !all(is.na(.x))))
df %>% left_join(SVIs, "SpectraID") %>% nest(SVI = NDVI:last_col()) -> df 

## MSC =============================================
df %>% select(spectra) %>% unnest(spectra) %>% as.matrix() %>% 
  msc %>% 
  as.matrix() %>% 
  as_tibble() %>% 
  mutate(across(`365`:`2500`, as.numeric))->
  msc.spectra
df %>% bind_cols(msc.spectra) %>% nest(MSC = `365`:`2500`) -> df

remove(msc.spectra)
gc()
## Derivatives ====================================================================
# Convert to long format
df_long <- df %>% select(SpectraID, spectra) %>% unnest(spectra) %>% 
  pivot_longer(`365`:`2500`, names_to = "Wavelength", values_to = "Reflectance") %>%
  mutate(Wavelength = as.numeric(Wavelength)) %>%
  arrange(SpectraID, Wavelength)

# Apply Savitzky-Golay derivatives per sample
df_deriv <- df_long %>%
  group_by(SpectraID) %>%
  mutate(
    D1  = sgolayfilt(Reflectance, m = 1),
    D2 = sgolayfilt(Reflectance,  m = 2)
  ) %>%
  ungroup() 

df_deriv %>% select(-D1) %>% 
  pivot_wider(id_cols = SpectraID, 
              names_from = Wavelength, 
              values_from = D2) -> D2

df_deriv %>% select(-D2) %>% 
  pivot_wider(id_cols = SpectraID, 
              names_from = Wavelength, 
              values_from = D1) -> D1

df %>% left_join(D1, "SpectraID") %>% nest(D1 = `365`:`2500`) %>% 
  left_join(D2, "SpectraID") %>% nest(D2 = `365`:`2500`) %>% 
  relocate(Treatment,spectra, MSC, D1, D2, SVI, 
           .after = SpectraID) -> df

remove(D1, D2, df_deriv, df_long)
gc()

## Plots ===================================================================== 
linewidth = 1
plot.spectra.df.mean(df %>% select(SpectraID, Treatment, spectra) %>% 
                       unnest(spectra), colour_by = Treatment, linewidth = linewidth) +
  ylab("Reflectance (Raw)")

plot.spectra.df.mean(df %>% select(SpectraID, Treatment, MSC) %>% 
                       unnest(MSC), colour_by = Treatment, linewidth = linewidth) +
  ylab("MSC-Reflectance")

plot.spectra.df.mean(df %>% select(SpectraID, Treatment, D1) %>% 
                       unnest(D1), colour_by = Treatment, linewidth = linewidth) +
  ylab("Reflectance (First Derivative)")

plot.spectra.df.mean(df %>% select(SpectraID, Treatment, D2) %>% 
                       unnest(D2), colour_by = Treatment, linewidth = linewidth) +
  ylab("Reflectance (Second Derivative)")

## wrangle data to work with the rest of the code ====================================
df %>% pivot_longer(spectra: SVI, names_to = "Correction", values_to = "data") %>% 
  dplyr::filter(Correction != "spectra") %>% 
  select("Mean",  "Date1","TreeID","Nutrient", "CIF",   "Index","Treatment", 
         "Cultivar","Concentration", "Scan","Experiment","filetype","Version",   "File Name",
         "Instrument","Detectors", "Measurement", "Date", "Time",  "Battery Voltage", 
         "Averages",  "Integration1",   "Integration2",     "Integration3",   
         "Dark Mode", "Foreoptic", "Radiometric Calibration","Units"  ,
         "Latitude","Longitude","Altitude","GPS Time","Satellites",
         "Calibrated Reference Correction File","Channels", "Correction", "data") %>% 
  group_by(Nutrient) %>% 
  mutate(Correction = factor(Correction, levels = c("MSC", "D1", "D2", "SVI"))) %>% 
  group_split() %>% `names<-`(c("N", "P")) %>% 
  lapply(\(x) x %>% group_by(Correction) %>% group_split %>% 
           `names<-`(c("msc", "d1", "d2", "vi"))) -> data_sets

#check that spectra and dataset line up
#data_sets$N$d2 %>% unnest(data) %>%  plot.spectra.df.mean(colour_by = Treatment)
saveRDS(data_sets, "data_sets")
remove(df)
gc()

data_sets$N %>% map(\(x) x %>% rename(N = Concentration)) -> data_sets$N
data_sets$P %>% map(\(x) x %>% rename(P = Concentration)) -> data_sets$P
#dimensions
select <- dplyr::select # make sure select is dplyr::select etc.
filter <- dplyr::filter
#ranges of N and P
data_sets$N[["msc"]] %>% select(N) -> N
(range(N) %>% diff()) / mean(N$N)

data_sets$P[["msc"]] %>% dplyr::filter(Mean == "arithmetic") %>% select(P) %>% na.omit -> P
(range(P) %>% diff()) / mean(P$P, na.rm = T)

# option Save datasets and load so that above code doesn't all need to be run
# saveRDS(data_sets, "data_sets")
# data_sets <- readRDS("data_sets")


#   Training and Test Sets =========================================
library(janitor)
library(caTools)

#expand each dataset
data <- data_sets %>% map(\(x) x %>% map(\(y) y %>% unnest(data)))
remove(data_sets)
gc()

#unique trees 
trees <-
  data %>% unlist(F) %>%  bind_rows() %>% dplyr::select(TreeID) %>% 
  unique() %>% mutate(TreeID = as.numeric(TreeID)) %>% arrange(TreeID) %>% 
  rename(training_tree = TreeID)
trees <- trees$training_tree

# all trees are present
trees == 1:150 #not relevant when working with a sample 

#split
split <-
  sample.split(levels(as.factor(trees)), SplitRatio = .8) # Split trees into training and test

training_trees <- trees[split == T] %>%
  as.data.frame() #split the trees into 3/4 training and 1/4 test
colnames(training_trees) <- "TreeID"
training_trees <-
  training_trees %>%  mutate(TreeID = as.character(TreeID))

# saveRDS(training_trees, "training_trees")

# there seems to be a problem here where setting the seed does not ensure identical
# results. As an alternative, training and test trees can be saved as an .RDS
# file and read into R to ensure identical results each time. 
# saved as RDS objects in the appropriate folder
# training_trees <- readRDS("training_trees")

test_trees <-
  trees %>% as.data.frame() %>% `colnames<-`("TreeID") %>%
  dplyr::filter(!(TreeID %in% training_trees$TreeID))
test_trees <-
  test_trees %>%  mutate(TreeID = as.character(TreeID))

# saveRDS(test_trees, "test_trees")
#option: load defined test trees (for reproducibility)
#test_trees <- readRDS("test_trees")

#merge with appropriate data
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



##Functions to partition data into different sets.==============

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
      }, `365`:`2500`) %>%
      na.omit()
  }
  PLS.columns.vi <- function(dataset, response) {
    dataset %>%
      dplyr::select({
        {
          response
        }
      }, NDVI:last_col()) %>%
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
# is supposed to).to replicate our results exactly  load the trained 
# model saved as an RDS file.
# ATTENTION! Running the job with the saveRDS line uncommoneted,
# will overwrite this file.

library(job)
# job({ #uncomment to run as a background job
trained.models.pls.all.dates <-
  lapply(training.sets.columns$byMean$training_columns, function(list2) {
    lapply(list2, function(list1) {
      lapply(list1, PLS.train)
    })
  })
saveRDS(trained.models.pls.all.dates,
        "trained.models.pls.all.dates")
# }, import = "auto") #trained.models.pls is too large we will have to find another solution: find best preprocessing using a single full dataset

#BM
trained.models.pls.all.dates <-
  readRDS("trained.models.pls.all.dates")
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

# Which pre-process performed best
(
  best.preprocessing <-
    results %>% mutate(is.vi = id == "vi") %>%  relocate(is.vi, .after = id) %>%
    group_by(Nutrient, is.vi) %>%
    slice_min(RMSE, n = 1)
)

results %>% group_by(Nutrient) %>% slice_max(Rsquared, n = 1) #this agrees
best.results <-
  results %>% group_by(Nutrient, id, Mean) %>% slice_max(Rsquared, n = 1) 

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


#plot CV results
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
                   `P` = "P"
                 ))) +
    theme_classic() + 
    scale_fill(labels = c("MSC", "SVI"))+
    scale_linetype(labels = c("MSC", "SVI"))+
    labs(x = "Latent Variables",
         y = "RMSE (Repeated CV)"))


ggplot(data = best.results %>% filter(Mean == "arithmetic"), aes(x = id, y = RMSE)) +
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
# theme(legend.key.size = unit(1, 'cm')) +
# theme(text = element_text(size = 12))

# Select best performing models
# The effect of mean is negligible. Random effects might change the outcome.
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
  mapply(predict, best.models.all.dates, best.test.byMean) %>%
  lapply(as_tibble) %>% bind_rows(.id = "id") %>%
  dplyr::rename(Predicted = value)
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

metrics <-
  data %>% group_by(Nutrient, Correction) %>%
  summarise(RMSE = caret::RMSE(Predicted, value),
            Rsquare = caret::R2(Predicted, value))

metrics.no.ol <-
  data %>% dplyr::filter(is.na(outlier)) %>% group_by(Nutrient, Correction) %>%
  summarise(RMSE = caret::RMSE(Predicted, value),
            Rsquare = caret::R2(Predicted, value))

#plot predictions against observed
library(ggpubr)
library(ggh4x)

# functions to assist with plotting
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
    geom_abline() +
    #geom_text(aes(label = TreeID), vjust = 1.2, hjust = 1.2) +
    xlab("Predicted Nutrient Concentration (%)") +
    ylab("Measured Nutrient Concentration (%)") +
    facet_wrap_equal(Nutrient ~ Correction, scales = "free") +
    theme_classic() +
    scale_colour_manual(
      limits = c("cont", "APP", "N", "P", "MoP"),
      labels = c("Control", "APP", "-N", "-P","Low P"),
      values = pal_jco()(6)[-4]
    ) +
    scale_shape(
      limits = c("cont", "APP", "N", "P", "MoP"),
      labels = c("Control", "APP", "-N", "-P","Low P")
    ) +
    stat_cor() +
    stat_regline_equation(vjust = 2, aes(label =  ..adj.rr.label..)) +
    theme(text = element_text(size = 18)) ->
    test.plots)

# scalar = 1.2
# ggsave("Figure_2.pdf", test.plots,
#        height = scalar * 6,
#        width = scalar * 8)

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


## Relationship between N and P ------------------------------------------

# interesting: prediction does poorly on leaves with low N
# is P underestimated in trees absent of N
library(dplyr)
data %>% filter(Nutrient == "P", Treatment == "N") %>% group_by(Correction) %>% do(t.test = t.test(Pair(value, Predicted) ~ 1, data = .data)) -> t.tests
t.tests$t.test

#is there a difference in the amount of underestimation between
# the SVI and MSC models
t.test(Pair(value, Predicted) ~ Correction,
       data = data %>% filter(Nutrient == "P", Treatment == "N")) ->
  t.test.comparison
t.test.comparison

remove(trained.models.pls.all.dates)
gc()

#PLS by Date -------------------------------------------------

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

#this must be changed manually
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

#same applies here as above: can load trained model
# job({
trained.models.pls.by.date <-
  lapply(selected.preprocessing, function(list2) {
    lapply(list2, PLS.train)
  })
saveRDS(trained.models.pls.by.date, "trained.models.pls.by.date")
# }, import = "auto")

trained.models.pls.by.date <- readRDS("trained.models.pls.by.date")
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

CV.by.date %>% mutate(across(RMSE:MAESD, \(x) round(x, 2)))

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
    "data" = data %>% select(Predicted, N, Treatment)
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
    "data" = data %>% select(Predicted, P, Treatment)
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


# merge all test predictions into one 
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

#get p.values
library(stats)
library(broom)
plotting.data %>% group_by(Model, Month) %>%
  do(lm = lm(Measured ~ Predicted, data = .data) %>%
       summary) %>%
  mutate(lm = list(tidy(lm))) %>% 
  unnest(lm) %>% filter(term == "Predicted") %>%
  mutate(p.adj = p.adjust(p.value, "holm")) %>%
  mutate(
    sig = ifelse(p.adj < .05, "*", NA),
    p.adj = round(p.adj, 3),
    p.format = case_when(p.adj < 0.001 ~ "italic('p') <.001", 
                         p.adj >= 0.001 ~ paste0("italic('p') == ", p.adj))) %>% 
  separate(Model, into = c("Nutrient", "Preprocess")) %>% 
  unite(Model, Nutrient, Preprocess, sep = ":") -> lm.by.month

library(ggh4x)

###plot results ===================================

#fixed slope
plotting.data %>% left_join(lm.by.month) %>%
  group_by(Model, Month) %>%
  mutate(p.format = if_else(row_number() == 1, p.format, NA)) %>% 
  group_by(Nutrient) %>% 
  do(
    plots =   ggplot(., aes(x = Predicted,  y = Measured)) +
      geom_point(aes(shape = Treatment, colour = Treatment)) +
      facet_wrap_equal(ncol = 3,
                       Model ~ Month) +
      theme_classic() +
      scale_colour(
        limits = c("APP", "cont", "MoP", "N", "P"),
        labels = c("APP", "Control", "Low P", "-N", "-P")
      ) +
      scale_shape_discrete(
        limits = c("APP", "cont", "MoP", "N", "P"),
        labels = c("APP", "Control", "Low P", "-N", "-P")
      ) +
      geom_abline() +
      #stat_cor() +  #uncomment for statistics
      #stat_regline_equation(vjust = 2, aes(label =  ..adj.rr.label..)) +
      xlab("Predicted Nutrient Concentration (%)") +
      ylab("Measured Nutrient Concentration (%)") +
      geom_text(aes( label = p.format),
                x = Inf,
                y = -Inf,
                size = 3,
                colour = "black",
                hjust = 1.5,
                vjust = -0.5,
                parse = T)) ->
  testsbydate.plot2

(testsbydate.plot2$plots %>% ggarrange(plotlist = .,
                                       common.legend = T,
                                       nrow = 2) -> testsbydate.plot2)

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


library(tidyverse)
library(ggpubr)

Metrics %>% 
  mutate(Not.trustworthy = ifelse(Nutrient == "P" &
                                    Date == "2023-05-02", RMSE,  0)) %>% 
  mutate(Date = as.Date(Date)) %>% 
  as_tibble ->
  Metrics #add column to allow exclusion of untrustworthy data

ggplot(data = Metrics) +
  geom_line(aes(x = Date,
                y = RMSE,
                colour = Preprocess),
            stat = "identity",
            # position = "dodge",
            linewidth = 1) +
  facet_wrap( ~ Nutrient, nrow = 2, scales = "free") +
  theme_classic() +
  scale_colour(limits = c("msc", "vi"),
               labels = c("MSC", "SVI")) +
  scale_x_date(date_labels = "%b %d",
               date_breaks = "3 weeks")+
  ylab("RMSE (Test)") +
  ylim(c(0,NA))+
  theme(
    text = element_text(size = 12),
    legend.position = c(0.27, 0.62),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.box.background = element_rect(colour = "black", linewidth = 1)
  ) -> RMSE.Date.Comparison

#metrics
(Metrics %>% relocate(Nutrient, Date) %>%
    mutate(Preprocess = str_replace_all(Preprocess, 
                                        pattern = c("msc" = "MSC", "vi" = "SVI"))) %>% 
    mutate(Month = month(Date, label = T, abbr = F) %>%
             as.character()) %>% 
    left_join(lm.by.month %>% separate(Model, into = c("Nutrient", "Preprocess")),
              by = c("Nutrient", "Preprocess", "Month")) %>%
    select(-c(Not.trustworthy, Month:sig)))


#using facet_wrap_equal
testsbydate.plot2 +
  theme(legend.position = "none",
        axis.text = element_text(size = 8)) -> x
RMSE.Date.Comparison -> y
#theme(legend.position = "none") -> y
(ggarrange(x, y, labels = "auto", widths = c(4.5, 2)) -> 
    testsbydate.plot.final)

# scalar = 1
# ggsave("Figure_5.pdf", testsbydate.plot.final,
#        height = scalar * 8,
#        width = scalar * 8)

remove(trained.models.pls.by.date)
gc()
#       Important Wavelengths      ============================================

library(plsVarSel)

# get data from best performing full-year models
bestTunes <-
  best.models.all.dates %>% get_elements("bestTune") %>% unlist()
finalModels <-
  best.models.all.dates %>% get_elements("finalModel")
trainingDatas <-
  best.models.all.dates %>% get_elements("trainingData") %>% lapply(function(x) {
    x[".outcome"] <- NULL
    x
  })

#get VIP
mapply(VIP, finalModels, bestTunes) %>%
  lapply(as_tibble, rownames = "Wavelength") %>%
  bind_rows(.id = "id")  %>%
  separate(id, c("Nutrient", "Correction")) %>%
  group_by(Nutrient, Correction) %>%
  dplyr::rename(VIP = value) %>%
  slice_max(VIP, n = 100) %>%
  mutate(Wavelength = gsub("`", "", Wavelength)) -> important.wavelengths.all.dates

#plot VIP
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

imp.SVI.VIP.mRMR %>% group_by(Nutrient) %>%  arrange(mRMR.rank) %>% 
  slice_min(mRMR.rank, n = 13) # %>% View()

## plot importance against spectra =========
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

data_sets <- readRDS("data_sets")
bind_rows(
  N =  data_sets$N$msc %>% unnest(data),
  P = data_sets$P$msc %>% unnest(data),
) %>%
  filter(Mean == "arithmetic")  %>% pivot_longer(`365`:`2500`, names_to = "Wavelength", values_to = "Reflectance") %>%
  filter((Nutrient == "N"  &
            Treatment %in% c("cont", "N", "LN")) |
           (Nutrient == "P"  &
              Treatment %in% c("cont", "P", "MoP"))|
           (Nutrient == "Multiclass")) %>% 
  mutate(Nutrient = factor(Nutrient,levels = c("N", "P", "Multiclass"))) -> data.to.plot


# install.packages("ggnewscale")
library(ggnewscale)

(ggplot.ribbon(data.to.plot, alpha = 0 ) +
    geom_vline(
      data = imp.msc,
      aes(xintercept = Wavelength),
      colour = "darkgrey" ,
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
    new_scale_colour() +
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
    facet_wrap(~Nutrient, nrow = 3) 
  # scale_color_manual(values = colors)
  -> variable.importance)

#difference plot
bind_rows(
  N =  data_sets$N$msc %>% unnest(data),
  P = data_sets$P$msc %>% unnest(data)) %>% 
  filter(Mean == "arithmetic")-> x
# 1. Compute mean spectrum for control
mean.spectra <- x %>% 
  filter(Treatment == "cont") %>% 
  pivot_longer(cols = `365`:`2500`, names_to = "Wavelength", values_to = "MSC") %>%
  group_by(Wavelength) %>%
  summarise(mean_MSC = mean(MSC, na.rm = TRUE), .groups = "drop")

# 2. Calculate difference from control spectrum
diff.spectra <- x %>%
  filter(Mean == "arithmetic") %>%
  select(Scan, Treatment, Nutrient, Date, `365`:`2500`) %>%
  pivot_longer(cols = `365`:`2500`, names_to = "Wavelength", values_to = "MSC") %>%
  left_join(mean.spectra, by = "Wavelength") %>%
  mutate(diff = MSC - mean_MSC) %>%
  pivot_wider(names_from = Wavelength, values_from = diff, 
              id_cols = c(Scan, Treatment, Nutrient, Date)) -> data.to.plot

data.to.plot %>% plot.spectra.df.mean(colour_by = Treatment, facet = Nutrient, linewidth = 0.1, alpha = 0.01) +
  facet_wrap( ~ Nutrient, scales = "free",
              nrow = 2) +
  ylab(expression(Reflectance[trt] - Reflectance[Control])) +
  scale_fill(
    labels = c("Control", "APP", "-N" , "Low N", "-P", "Low P"),
    limits = c("cont", "APP", "N", "LN", "P", "MoP")
  ) +
  scale_colour(
    labels = c("Control", "APP", "-N" , "Low N", "-P", "Low P"),
    limits = c("cont", "APP", "N", "LN", "P", "MoP")
  ) +
  geom_vline(
    data = imp.msc,
    aes(xintercept = Wavelength),
    colour = "darkgrey" ,
    alpha = .2,
    show.legend = F
  ) +
  geom_vline(
    data = imp.all.dates.mRMR %>% filter(Correction == "msc"),
    aes(xintercept = Wavelength %>% as.numeric()),
    alpha = .4,
    colour = "orange" ,
    show.legend = F
  ) +
  geom_line(data = mean.spectra %>% mutate(Wavelength = as.numeric(Wavelength)),
            aes(x = Wavelength, y = mean_MSC/10 + .1), colour = "black") +
  geom_line(data = data.to.plot %>% 
              pivot_longer(cols = `365`:`2500`, names_to = "Wavelength", values_to = "MSC") %>%
              group_by(Wavelength, Treatment, Nutrient) %>%
              summarise(mean_MSC = mean(MSC, na.rm = TRUE),
                        sd_MSC = sd(MSC, na.rm = TRUE)),
            aes(x = as.numeric(Wavelength), y = mean_MSC, colour = Treatment),
            linewidth = .8) -> Variabale.importance.plot

# scalar = 1
# ggsave("Figure_3.pdf", Variabale.importance.plot,
#        height = 6*scalar, width = 8*scalar)


remove(data_sets)
gc()

#  Backward Feature Elimination  =============================================

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
# considerable time.
# The results can be loaded from a saved object:

# job({
remove(model)
lapply(best.models.all.dates, function(model) {
  #model = best.models.all.dates$N.msc
  nsel <-
    c(2:30, seq(40, 100, 10), 109, seq(100, dim(model$trainingData)[2] -
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
# }, import = "auto")

# Get importance using VIP algorithm and data to plot
# job({
lapply(best.models.all.dates, function(model) {
  #model = best.models.all.dates$N.msc
  nsel <-
    c(2:30, seq(40, 100, 10), 109, seq(100, dim(model$trainingData)[2] -
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
# }, import = "auto")

# merge into onde data frame and save
RFE.results <- list(mRMR = learning.curve.data.mRMR,
                    VIP = learning.curve.data.VIP)
saveRDS(RFE.results, "RFE.results")
gc()

#load saved data files 
RFE.results <- readRDS("RFE.results")
learning.curve.data.mRMR <- RFE.results[["mRMR"]]
learning.curve.data.VIP <- RFE.results[["VIP"]]

# Plot Results
learning.curve.data.mRMR %>% bind_rows(.id = "id") %>%
  separate(id, into = c("Nutrient", "Preprocess")) -> data.to.plot.mRMR

learning.curve.data.VIP %>% bind_rows(.id = "id") %>%
  separate(id, into = c("Nutrient", "Preprocess")) -> data.to.plot.VIP

bind_rows(mRMR = data.to.plot.mRMR, VIP = data.to.plot.VIP, .id = "Sel_proc") ->
  data.to.plot

(data.to.plot %>% group_by(Nutrient) %>% 
    do(plot = ggplot(data = .,
                     aes(x = nsel)) +
         geom_ribbon(aes(
           ymin = RMSE - RMSESD,
           ymax = RMSE + RMSESD,
           fill = Sel_proc
         ),
         alpha = 0.3,
         ) +
         geom_line(aes(y = RMSE, lty = Sel_proc, colour = Sel_proc), linewidth = .8) +
         facet_grid(
           Nutrient ~ Preprocess,
           # scales = "free",
           labeller = labeller(Preprocess = c("msc" = "MSC", "vi" = "SVI")),
           # independent = "y"
         ) +
         theme_classic() +
         scale_fill_manual(name = "", values = c("Orange", "grey")) +
         scale_colour_manual(name = "", values = c("Orange", "grey")) +
         scale_linetype_discrete(name = "") +
         xlab("Number of Selected Features") +
         ylab("RMSE (Cross-Validated)") +
         theme(text = element_text(size = 12),
               legend.text = element_text(size = 12)) +
         xlim(c(0, 100)) -> rfe.plot))

library(dplyr)
library(ggplot2)

rfe.plot <- data.to.plot %>%
  group_by(Nutrient) %>%
  do(plot = ggplot(., aes(x = nsel)) +  # <-- use '.' inside do() to refer to the grouped data
       geom_ribbon(aes(
         ymin = RMSE - RMSESD,
         ymax = RMSE + RMSESD,
         fill = Sel_proc
       ), alpha = 0.3) +
       geom_line(aes(y = RMSE, lty = Sel_proc, colour = Sel_proc), linewidth = 0.8) +
       facet_grid(
         Nutrient ~ Preprocess,
         labeller = labeller(Preprocess = c("msc" = "MSC", "vi" = "SVI"))
         # independent = "y" # (this only applies to ggh4x::facet_grid2)
       ) +
       theme_classic() +
       scale_fill_manual(name = "", values = c("Orange", "grey")) +
       scale_colour_manual(name = "", values = c("Orange", "grey")) +
       scale_linetype_discrete(name = "") +
       xlab("Number of Selected Features") +
       ylab("RMSE (Cross-Validated)") +
       theme(text = element_text(size = 12),
             legend.text = element_text(size = 12)) +
       xlim(c(0, 100))
  )

rfe.plot$plot
(ggarrange(plotlist = rfe.plot$plot,
           nrow = 2,
           labels = NULL,
           common.legend = T,
           legend = "right"
) -> rfe.plot.final)

# scalar = 1
# ggsave("Figure_4.pdf", rfe.plot.final,
#        height = 6*scalar, width = 8*scalar)



