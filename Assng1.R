#####
#Assignment 1 - Software Tools 

#Introduction  

#This project centres around tardigrades in the family Echiniscidae.  Commonly known as the waterbears, these small, water-dwelling, segmented animals with eight legs are one of the most complex of all known polyextremophiles. These species are famous for their ability to withstand extreme conditions (Zawierucha et al. 2015). As a result they have been found to be distributed across all continents, inhabiting a variety of ecosystems. At present, 1200 species have been identified globally, yet this value is likely to be underestimated (Zawierucha et al. 2015). However, despite these numbers and their wide range of distribution, knowledge of their abundance and diversity along environmental and geographical gradients is still unclear (Gasiorek et al. 2021, Garey et al. 2008).

#In light of this uncertainty, this project will take an exploratory approach to investigate the diversity and distribution of tardigrades across different countries from which they’ve been sampled. Given their wide range of distributions, it is hypothesized that sampling efforts across different countries have been extensive for most genera of tardigrade within the Echiniscidae family. As well, diversity of genera sampled should be relatively equal, since nearly all genres of tardigrades are known to be widely distributed. 

#####
#The required packages were loaded into R. Tidyverse allows for easy data manipulation, exploration and visualization. Vegan is an R package for community ecologists. It contains the most popular methods of multivariate analysis for analyzing communities. The ggplot2 is a data visualization package that allows us to create complex plots. The labdvs package contains useful function that allow ordination and community analyses.

#* =zuhaa's edits
#*minor typo edit should be tidyverse instead of tideyverse

# Loading the packages needed for analysis 
install.packages("tidyverse")
library(tidyverse)
install.packages("labdsv")
library(labdsv)
install.packages("ggplot2")
library(ggplot2)
install.packages("vegan")
library(vegan)

#####

#Using an API call for the Tardigrade dataset. The call has been customized to focus on the family Echiniscidae. 

#Calling the dataset for the tardigrades in the family Echiniscidae.
Tardigrade<- read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Echiniscidae&format=tsv")
#*should be called Tardigrade instead of Tradigrad

#To narrow our focus to different genera of Echiniscidae and the countries they’re sampled from, a new data frame is created (grouped_Data). All missing entries from countries and genus are filtered out. 
#Creates data frame for unique combination of countries and genus along and tallies number of genus from that country. All unknown countries and genus are filtered out. 
#grouped_Data = Tardigrade%>%
  #filter(!is.na(country)) %>%
  #filter(!is.na(genus_name)) %>%
  #group_by(country, genus_name) %>%
  #tally() 
#####
#Refraction Curve (Figure 1)
#To get a sense of sampling completeness of genera sampled refraction curve is created.

#Reorganizing the grouped_Data tidy data into basic dataframe 
#data_frame_grouped = data.frame(country=grouped_Data[,1],
                                #genus = grouped_Data[,2],
                                #obsv = grouped_Data[,3])
#*data.frame can easily convert tibble to dataframe saving us time instead of  writing the code above
#data_frame_grouped= data.frame(grouped_Data)

#*make sure to view after data filtration step to ensure it worked like you intended
#View(data_frame_grouped)

#*ideally we would want the genus name, followed by country, followed by sample size
#rearranging column names
#data_frame_grouped <- data_frame_grouped[, c("genus_name","country","n")]

#*we would also want column n to have a proper name so it is easier to identify what the column represents
#colnames(data_frame_grouped)[colnames(data_frame_grouped)=="n"] <- "sample_size"
#*view again to ensure the above step worked
#View(data_frame_grouped)


##* Creating function to do all of the above so any data can be subsampled in lesser lines of code and has increased generality and usability 

subset_data <- function (dataset) {
    
  x<-data.frame(dataset%>%
    filter(!is.na(country)) %>%
    filter(!is.na(genus_name)) %>%
    group_by(country, genus_name) %>%
    tally() %>% rename("sample_size"="n"))
  
      y <- x[, c("genus_name","country","sample_size")]
      
  }
data_frame_grouped<-subset_data(Tardigrade)
View(data_frame_grouped)

#* Adding a frequency distribution curve for sample size
#* This shows that the sample sizes are very unbalanced among genus, where quite a few genus were only sampled once or twice whereas others may have been sampled > than 100 times. It might be better to choose a dataset that is more balanced to determine the diversity aspect of the question
hist(data_frame_grouped$sample_size,breaks = 40, xlab ="sample size",main = "Genus sample size" )

#matrify requires the labdsv package. The function rearranges the data-frame data_frame_grouped into a numeric matrix.
comm_mat = matrify(data_frame_grouped)

#col <- c("black", "darkred", "forestgreen", "orange", "blue", "yellow", "hotpink")

#Creating a refraction curve using the transpose of the community matrix, so genera would be depicted as a function of countries. 
refrac_curve = rarecurve((comm_mat), col=col, label=F, ylab="Genera", main ="Refraction curve of Genera across Countries", tidy=TRUE)

#*should say refraction rather than refranction curve

#*#*Matrify takes 3 columns in a dataframe  in the form of sample id, taxon and abundance and converts to full matrix form. Rearranging column names to an ideal arrangement no longer produces the same output for refraction curve so we need to switch the rows and columns and so we must remove t/transpose in our rarecurve code.

#*The labels are overlapping the axes and other genus. It would be better to have the genus names as a legend on the side and wider lines so they are visible, this is not possible with rarecurve() however, rarecurve can return a tidy dataframe (tidy=TRUE) that can be used with ggplot to get better graphics
#*To make the colors accessible to everyone a colorblind friendly palette found from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/ was used
#*col=col can be taken out from the above code 

##*improved rarefraction curve

ggplot(refrac_curve, aes(x = Sample, y = Species, color = Site))+ ylab("Genera") + xlab("Sample_size") + theme_bw() +geom_line(size=1.2) + labs(title = "Tarigrade Genus Distribution across Countries")+ theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) +  scale_color_manual(values=c("#999999", "#56B4E9", "#E69F00", "#009E73", "#CC79A7", "#D55E00"))

#Figure 1 depicts the refraction curve of sampled genera across countries. The slope of the curves are indicators of sampling completeness per genera across all countries they’ve been sampled from. 

#####
#Stacked Bar-chart (Figure 2)

#Creating a stacked bar chart for each country and the number of specimen per genus sampled from said country.

#*Creating stack bar-plot of of genera per country. The theme() function flips the x-axis labels to a vertical position.  
ggplot(grouped_Data, aes(x =country, y= n, fill= genus_name)) + geom_col() + scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00", "#009E73", "#CC79A7", "#D55E00")) + labs(title = "Tarigrade Genus Distribution", x = "Countries", y = "Count")+ theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##*improving figure 2
#*Title only needs to depict what is happening in the graph rather than defining the type of graph it is so having Tardigrade Genus Distribution is probably a better fitted title than Stacked Barchart of Tarigrade Genus Distribution.Ideally you would want the title to be centered rather than left justified. You can do this by using plot.title = element_text(hjust = 0.5) within theme of ggplot.

#*Shades of green, blue and turquoise may seem similar to people who are colorblind, using color palettes that are easily distinguishable and clear are important to use so you figures are accessible to all. Colors can be assigned using scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00", "#009E73", "#CC79A7", "#D55E00")). Here I used a colorblind friendly palette found from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/ 


#Figure 2 depicts frequency of genera from the family Echiniscidae sampled per country. This allows for easy visualization of sampled genera diversity across countries.

#####
#Diversity Analysis (Table 1)

#Diversity function requires a numeric matrix (comm_mat). The function is using the Shannon-Wiener diversity index for analysis. 

#Diversity analysis of genera per country
diversity(comm_mat)

#Table 1 depicts the results of the diversity analysis. The higher the values per country, the greater the genera sample diversity within the country. 
#####

# Accumulation curve (Figure 3)

#A speices accumualtion curve. Resampling of sites is performed to see how genera accumulate as sites are added (countries are treated as sites) 

#Finds species accumulation curves or the number of species for a certain number of sampled sites or individuals.
AccumCurve <- specaccum(comm_mat)
#Creating new data.frame using the results from the accumulation curve (AccumCurve),
accum_frame <- data.frame(Sites=AccumCurve$sites, Richness=AccumCurve$richness, SD=AccumCurve$sd)

#Plotting the accumulation curve for genera across countries.
ggplot() +
  geom_point(data=accum_frame, aes(x=Sites, y=Richness)) +
  geom_line(data=accum_frame, aes(x=Sites, y=Richness)) +
  geom_ribbon(data=accum_frame ,aes(x=Sites, ymin=(Richness-2*SD), ymax =(Richness+2*SD)),alpha=0.2) +labs(title="Accumulation curve for highly sampled Genus", x="Countries Sampled", y="Genus Richness")

#Figure 3 shows the accumulation curve for genera across all countries. Sites are resampled see how genera accumulate as sites. In this case countries are treated as sites.

#####
#Discussion

#To better understand sampling efforts across genera, a refraction curve was plotted. The slope of the most of the genera are all positive, with no indication of plateau. Several genera lacked a curve completely, indicating a need for improved sampling efforts. Most notable, sampling within the genus Echiniscus, Pseudechiniscus and Diploechiniscus is more extensive then other. In addition, the accumulation curve (Figure 3) reveals that despite the increase in countries the genera richness appears to be in an upward trend, with no sign of plateau. This indicates that additional samples from countries need to be added to ensure complete sampling of this famil. However, from these figure along, it’s unclear how the sampling effort varies across countries

#Figure 2 overviews how the genera are distributed across countries. A stacked bar chart depicts this distribution. The bar chart indicates that sampling efforts for most genus of Echiniscidae are not equal across countries, disproving our initial hypothesis. It also highlights that the genera Echiniscus and Pseudechiniscus, are sampled more often on average. This corroborates the positive trends observed in Figure 1 for both genera. In addition, Spain, Norway, Portugal and Italy appear to be providing the majority of these specimen. However, despite there higher sampling frequency, there is a lack of genera diversity within the samples. Countries only have samples of one or two genera, indicated a lack of genus diversity sampled in these countries, despite the abundance of samples.

#To better explore the diversity of genera across countries a diversity analysis uses vegan packages was conducted. There are various way to calculate diversity in biological communities. The simplest being “species richness” where the total number of species present across a community are recorded (Garey et al. 2008). However, this method ignores both the identity of the species and their abundances. To take into account the differences in abundance of each genera across each country, Shannon-Wiener diversity indices were determined. This approach required a numeric matrix with countries as row entries and genera as column entries (comm_mat). Cells show the respective abundance of each genus per country. The results show that while countries may have an abundance of samples, there samples don't reflect the diversity of the genera. For example, Spain has one of the highest frequency of samples, yet the results show 0% diversity across the samples. This is also evident from the bar-chart which reveals that Spain only contained samples of Echiniscus. In contrast, Norway, which also has a high frequency of samples, has a diversity score of 0.8987354, indicating a higher abundance of genera being sampled. This is also evident in the bar chart where multis genera are shown to be abundantly sampled. In addition, Egypt has a relatively high diversity index (0.6365142), despite the limited number of samples shown in the bar chart. This also disproves our hypothesis suggesting equal distribution of genre across countries. 

#Overall, the analysis coupled with the figures indicated a need for improved sampling efforts within this taxa. Despite the abundance and extreme range of these species very little specimens have been sampled and studied indicating a need for further sampling efforts. 

#####

#References 

#Garey J.R., McInnes S.J., Nichols P.B.(2008) Global diversity of tardigrades (Tardigrada) in freshwater. Hydrobiologia. 595: 101–106.

#Gąsiorek, P., Vončina, K., Zając, K., Michalczyk, L. (2021) Phylogeography and morphological evolution of Pseudechiniscus (Heterotardigrada: Echiniscidae). Scientific Repport 11(7606). https://doi.org/10.1038/s41598-021-84910-6.

#Zawierucha K., Smykla, J., Michalczyk, L., Gołdyn, B., Kaczmarek, L. (2015) Distribution and diversity of Tardigrada along altitudinal gradients in the Hornsund, Spitsbergen (Arctic), Polar Research, 34(1). DOI: 10.3402/polar.v34.24168.
