#IJPH assessment
library(data.table)
library(R.utils)
library(ggplot2)
library(scales)
library(ggridges)
ijph_orig <- fread("https://github.com/raysoumalya/RPublichealth/raw/main/IJPH_Bibliometric_Analysis/IJPH_Bibliometric_Pubmed.csv.gz")
ijph_orig <- setDT(ijph_orig)[`Publication Year`<2022,]
#class(ijph_orig)
ijph_orig[,suppl:=fifelse(`Type_Journal_Issue`=="suppl",1,0)]
time_publish_1 <- ijph_orig[,.N,by=`Publication Year`][order(`Publication Year`)]
time_publish_2 <- ijph_orig[suppl==0,][,.N,by=`Publication Year`][order(`Publication Year`)]

time_publish_2[, roll_mean := shift(frollmean(N, 10),-5)]

ggplot(time_publish_1[`Publication Year`%%5==1], aes(x=`Publication Year`)) + 
  geom_line(aes(x=`Publication Year`,y=`N`), time_publish_1, color='red') +  
  geom_line(aes(x=`Publication Year`,y=`N`), time_publish_2, color = 'blue') +
  geom_line(aes(x=`Publication Year`,y=`roll_mean`), time_publish_2, color='black', size=1.4) +
  geom_label(aes(y=N,label=N),
             position = position_dodge(width=0.9), size = 4) + 
  ylab("Number of Publication") + scale_x_continuous(breaks = pretty_breaks(8)) +
  theme(text = element_text(80))

author1<-ijph_orig[,.N,by=Author1]
colnames(author1)<- c("authorname","count1")
author1$authorname<-gsub("[[:punct:]]","",
                                 as.character(author1$authorname))
author1$authorname<-trimws(author1$authorname)
author2<-ijph_orig[,.N,by=Author2]
colnames(author2)<- c("authorname","count2")
author2$authorname<-gsub("[[:punct:]]","",
                         as.character(author2$authorname))
author2$authorname<-trimws(author2$authorname)
author3<-ijph_orig[,.N,by=Author3]
colnames(author3)<- c("authorname","count3")
author3$authorname<-gsub("[[:punct:]]","",
                         as.character(author3$authorname))
author3$authorname<-trimws(author3$authorname)
author4<-ijph_orig[,.N,by=Author4]
colnames(author4)<- c("authorname","count4")
author4$authorname<-gsub("[[:punct:]]","",
                         as.character(author4$authorname))
author4$authorname<-trimws(author4$authorname)
author5<-ijph_orig[,.N,by=Author5]
colnames(author5)<- c("authorname","count5")
author5$authorname<-gsub("[[:punct:]]","",
                         as.character(author5$authorname))
author5$authorname<-trimws(author5$authorname)
author6<-ijph_orig[,.N,by=Author6]
colnames(author6)<- c("authorname","count6")
author6$authorname<-gsub("[[:punct:]]","",
                         as.character(author6$authorname))
author6$authorname<-trimws(author6$authorname)
author7<-ijph_orig[,.N,by=Author7]
colnames(author7)<- c("authorname","count7")
author7$authorname<-gsub("[[:punct:]]","",
                         as.character(author7$authorname))
author7$authorname<-trimws(author7$authorname)
author8<-ijph_orig[,.N,by=Author8]
colnames(author8)<- c("authorname","count8")
author8$authorname<-gsub("[[:punct:]]","",
                         as.character(author8$authorname))
author8$authorname<-trimws(author8$authorname)
author9<-ijph_orig[,.N,by=Author9]
colnames(author9)<- c("authorname","count9")
author9$authorname<-gsub("[[:punct:]]","",
                         as.character(author9$authorname))
author9$authorname<-trimws(author9$authorname)
author10<-ijph_orig[,.N,by=Author10]
colnames(author10)<- c("authorname","count10")
author10$authorname<-gsub("[[:punct:]]","",
                         as.character(author10$authorname))
author10$authorname<-trimws(author10$authorname)
author11<-ijph_orig[,.N,by=Author11]
colnames(author11)<- c("authorname","count11")
author11$authorname<-gsub("[[:punct:]]","",
                         as.character(author11$authorname))
author11$authorname<-trimws(author11$authorname)

author_final <- author11[author10[author9[author8[author7[author6[author5[author4[author3[author2[author1, on = "authorname"]
        [, unique(c(count1, count2)), by = authorname], 
        on = "authorname"][, unique(c(V1, count3)), by = authorname], 
        on = "authorname"][, unique(c(V1, count4)), by = authorname], 
        on = "authorname"][, unique(c(V1, count5)), by = authorname], 
        on = "authorname"][, unique(c(V1, count6)), by = authorname], 
        on = "authorname"][, unique(c(V1, count7)), by = authorname], 
        on = "authorname"][, unique(c(V1, count8)), by = authorname], 
        on = "authorname"][, unique(c(V1, count9)), by = authorname], 
        on = "authorname"][, unique(c(V1, count10)), by = authorname], 
        on = "authorname"][, unique(c(V1, count11)), by = authorname]

#author_final[authorname=="Zodpey SP"|authorname=="Zodpey S",]
author_final[,.N,by=V1]

author_total_publication <- head(author_final[, sum(V1,na.rm = T),by=authorname][order(-V1)],20)
ggplot(data=author_total_publication[authorname>0,], 
       aes(x=reorder(authorname, V1), y=V1, label = V1)) + 
  geom_bar(stat="identity")+coord_flip()+xlab("Authors") + 
  ylab("Number of Publications") + geom_text(hjust = -0.1) +
  theme(text = element_text(100),
        axis.text.y = element_text(face = "bold", size = 10))
ijph_orig[,authorname2:=fifelse(grepl("Biswas R", Authors),"Biswas R","0")]
ijph_orig[,authorname2:=fifelse(grepl("Seal SC", Authors),"Seal SC",authorname2)]
ijph_orig[,authorname2:=fifelse(grepl("Ray SK", Authors),"Ray SK",authorname2)]
ijph_orig[,authorname2:=fifelse(grepl("Zodpey SP|Zodpey S", Authors),"Zodpey SP",authorname2)]
ijph_orig[,authorname2:=fifelse(grepl("Kant S", Authors),"Kant S",authorname2)]
ijph_orig[,authorname2:=fifelse(grepl("Nandan D", Authors),"Nandan D",authorname2)]
ijph_orig[,authorname2:=fifelse(grepl("Dobe M", Authors),"DObe M",authorname2)]
ijph_orig[,authorname2:=fifelse(grepl("Pandav CS", Authors),"Pandav CS",authorname2)]
ijph_orig[,authorname2:=fifelse(grepl("Kumar A", Authors),"Kumar A",authorname2)]
ijph_orig[,authorname2:=fifelse(grepl("Dasgupta S", Authors),"Dasgupta S",authorname2)]

ggplot(ijph_orig[authorname2>0,], aes(x = `Publication Year`, y = `authorname2`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1.8, rel_min_height = 0.01) + 
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) + scale_x_continuous(breaks = pretty_breaks(6)) + ylab("Name of the Authors") + 
  theme(text = element_text(80),
        axis.text.y = element_text(face = "bold", size = 10))

ggplot(data=ijph_orig[type_article>0,][,.N, by= type_article], 
       aes(x=reorder(type_article, N), y=N, label = N)) + 
  geom_bar(stat="identity")+coord_flip()+xlab("Type of Articles") + 
  ylab("Number of Articles") + geom_text(hjust = -1) +
  theme(text = element_text(50))

head(ijph_orig[!is.na(citation_count),c("Title","citation_count")][order(-citation_count)],10)

ggplot(data=head(ijph_orig[!is.na(citation_count),c("Title","citation_count")],10), 
       aes(x=reorder(`Title`, `citation_count`), y=`citation_count`, label = `citation_count`)) + 
  geom_bar(stat="identity")+coord_flip()+xlab("Articles") + 
  ylab("Number of Citations received")+ geom_text(hjust = 0.03) + 
  scale_x_discrete(labels = wrap_format(48)) + 
  theme(text = element_text(80),
        axis.text.y = element_text(face = "bold", size = 10))


authorno2 <- ijph_orig[AuthorNo<3,.N, by=`Publication Year`]
authorno4 <- ijph_orig[AuthorNo>3,.N, by=`Publication Year`]
authorno2_1 <- authorno2[time_publish_1, on = "Publication Year"]
authorno2_1[,pcnt:=round(N*100/i.N,0)][,pcnt:=fifelse(is.na(pcnt),0,pcnt)][,roll_pcnt:=round(shift(frollmean(pcnt,11),-5),0)]
authorno4_1 <- authorno4[time_publish_1, on = "Publication Year"]
authorno4_1[,pcnt:=round(N*100/i.N,0)][,pcnt:=fifelse(is.na(pcnt),0,pcnt)][,roll_pcnt:=round(shift(frollmean(pcnt,11),-5),0)]

ggplot(authorno4_1[`Publication Year`%%5==1,], aes(x=`Publication Year`))+
  geom_line(aes(x=`Publication Year`, y= roll_pcnt), authorno2_1, color='red') +
  geom_line(aes(x=`Publication Year`, y= roll_pcnt), authorno4_1) +
  geom_label(aes(y=N, label = roll_pcnt), vjust = -0.22,
             position = position_dodge(width=0.1), size = 5) + 
  theme(text = element_text(50)) +
  ylab("Moving Average of Percentage of Articles") + 
  scale_x_continuous(breaks = pretty_breaks(8)) + 
  theme(text = element_text(100))



median_citation_year <- ijph_orig[, median(citation_count,na.rm = T),
                                  by="Publication Year"][order(`Publication Year`)]
median_citation_year[, roll_mean := round(shift(frollmean(V1, 11),-5),0)]

ggplot(median_citation_year[`Publication Year`%%5==1,], aes(x=`Publication Year`))+ 
  geom_line(aes(x=`Publication Year`, y=V1), median_citation_year) + 
  geom_line(aes(x=`Publication Year`,y=roll_mean), median_citation_year, color = 'red') + 
  ylab("Median Number of Citation") + 
  scale_x_continuous(breaks = pretty_breaks(8)) + 
  geom_label(aes(y=roll_mean, label = roll_mean), vjust = -0.22,
             position = position_dodge(width=0.1), size = 5)+ 
  theme(text = element_text(100))

#COntributing author for most citation
#this will delete previous author characteristics
author1<-ijph_orig[citation_count>43,][,.N,by=Author1]
colnames(author1)<- c("authorname","count1")
author1$authorname<-gsub("[[:punct:]]","",
                         as.character(author1$authorname))
author1$authorname<-trimws(author1$authorname)
author2<-ijph_orig[citation_count>43,][,.N,by=Author2]
colnames(author2)<- c("authorname","count2")
author2$authorname<-gsub("[[:punct:]]","",
                         as.character(author2$authorname))
author2$authorname<-trimws(author2$authorname)
author3<-ijph_orig[citation_count>43,][,.N,by=Author3]
colnames(author3)<- c("authorname","count3")
author3$authorname<-gsub("[[:punct:]]","",
                         as.character(author3$authorname))
author3$authorname<-trimws(author3$authorname)
author4<-ijph_orig[citation_count>43,][,.N,by=Author4]
colnames(author4)<- c("authorname","count4")
author4$authorname<-gsub("[[:punct:]]","",
                         as.character(author4$authorname))
author4$authorname<-trimws(author4$authorname)
author5<-ijph_orig[citation_count>43,][,.N,by=Author5]
colnames(author5)<- c("authorname","count5")
author5$authorname<-gsub("[[:punct:]]","",
                         as.character(author5$authorname))
author5$authorname<-trimws(author5$authorname)
author6<-ijph_orig[citation_count>43,][,.N,by=Author6]
colnames(author6)<- c("authorname","count6")
author6$authorname<-gsub("[[:punct:]]","",
                         as.character(author6$authorname))
author6$authorname<-trimws(author6$authorname)
author7<-ijph_orig[citation_count>43,][,.N,by=Author7]
colnames(author7)<- c("authorname","count7")
author7$authorname<-gsub("[[:punct:]]","",
                         as.character(author7$authorname))
author7$authorname<-trimws(author7$authorname)
author8<-ijph_orig[citation_count>43,][,.N,by=Author8]
colnames(author8)<- c("authorname","count8")
author8$authorname<-gsub("[[:punct:]]","",
                         as.character(author8$authorname))
author8$authorname<-trimws(author8$authorname)
author9<-ijph_orig[citation_count>43,][,.N,by=Author9]
colnames(author9)<- c("authorname","count9")
author9$authorname<-gsub("[[:punct:]]","",
                         as.character(author9$authorname))
author9$authorname<-trimws(author9$authorname)
author10<-ijph_orig[citation_count>43,][,.N,by=Author10]
colnames(author10)<- c("authorname","count10")
author10$authorname<-gsub("[[:punct:]]","",
                          as.character(author10$authorname))
author10$authorname<-trimws(author10$authorname)
author11<-ijph_orig[citation_count>43,][,.N,by=Author11]
colnames(author11)<- c("authorname","count11")
author11$authorname<-gsub("[[:punct:]]","",
                          as.character(author11$authorname))
author11$authorname<-trimws(author11$authorname)

author_final <- author11[author10[author9[author8[author7[author6[author5[author4[author3[author2[author1, on = "authorname"]
                                                                                          [, unique(c(count1, count2)), by = authorname], 
                                                                                          on = "authorname"][, unique(c(V1, count3)), by = authorname], 
                                                                                  on = "authorname"][, unique(c(V1, count4)), by = authorname], 
                                                                          on = "authorname"][, unique(c(V1, count5)), by = authorname], 
                                                                  on = "authorname"][, unique(c(V1, count6)), by = authorname], 
                                                          on = "authorname"][, unique(c(V1, count7)), by = authorname], 
                                                  on = "authorname"][, unique(c(V1, count8)), by = authorname], 
                                          on = "authorname"][, unique(c(V1, count9)), by = authorname], 
                                  on = "authorname"][, unique(c(V1, count10)), by = authorname], 
                         on = "authorname"][, unique(c(V1, count11)), by = authorname]

#author_final[authorname=="Zodpey SP"|authorname=="Zodpey S",]
author_final[,.N,by=V1]

author_total_publication <- head(author_final[, sum(V1,na.rm = T),by=authorname][order(-V1)],20)
ggplot(data=author_total_publication[authorname>0,], 
       aes(x=reorder(authorname, V1), y=V1, label = V1)) + 
  geom_bar(stat="identity")+coord_flip()+xlab("Authors") + 
  ylab("Number of Publications") + geom_text(hjust = -0.1) +
  theme(text = element_text(100),
        axis.text.y = element_text(face = "bold", size = 10))


ijph_orig[,authorname:=fifelse(grepl("Sinha DN",Authors) & citation_count>43,"Sinha DN","0")]
ijph_orig[,authorname:=fifelse(grepl("Biswas R",Authors) & citation_count>43,"Biswas R",authorname)]
ijph_orig[,authorname:=fifelse(grepl("Balhara YPS",Authors) & citation_count>43,"Balhara YPS",authorname)]


ggplot(ijph_orig[authorname>0,], aes(x = `Publication Year`, y = `authorname`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1.8, rel_min_height = 0.01) + 
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) + ylab("Name of the Authors") + 
  theme(text = element_text(80),
        axis.text.y = element_text(face = "bold", size = 10))
#author_complete <- Reduce(function(...) merge(..., all = TRUE),
#                          list(author1,author2,author3,author4,author5,
#                               author6,author7,author8,author9,author10,author11))
#author_complete[,total:= rowSums(author_complete[,2:12],na.rm=TRUE)]
#setorder(author_complete,-total)

library(DiagrammeR)
grViz(diagram = "digraph flowchart {
  node [fontname = timesnewroman, shape = rectangle]
  tab1 [label = '@@1']
  tab2 [label = '@@2']
  tab3 [label = '@@3']
  
  tab1 -> tab3;
  tab2 -> tab3;
}
  
  [1]: 'Pubmed (2470)'
  [2]: 'Google Scholar (2499)'    
  [3]: '2470 articles with citation number'    
  ")
