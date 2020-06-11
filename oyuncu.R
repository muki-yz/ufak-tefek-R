library(ggplot2)
library(rvest)
library(dplyr)
link <- html("https://www.transfermarkt.com.tr/super-lig/marktwertspruenge/wettbewerb/TR1/ajax/yw1/page/1")

oyuncu_isim<-link%>%
  html_nodes("#yw1 .inline-table .hauptlink")%>%
  html_text(trim = TRUE)

pozisyon<-link%>%
  html_nodes("#yw1 .inline-table tr+ tr td")%>%
  html_text(trim = TRUE)
pozisyon<- as.factor(pozisyon)

yas<-link%>%
  html_nodes("#yw1 td:nth-child(4)")%>%
  html_text()
yas<-as.numeric(yas)

degisim<-link%>%
  html_nodes(".hauptlink+ .greentext")%>%
  html_text()

#de??i??im istedi??imiz gibi numerik gelmiyor % i??areti var
degisim<-as.numeric(sapply(strsplit(degisim, " "), "[[", 1)) 
#stringi bo??luktan ay??r??p ilk k??sm??n?? al??p onu numerik de??ere cevirdik, sapply ile bunu tum listeye uygulad??k


takim<-link%>%
  html_nodes("#yw1 .vereinprofil_tooltip")%>%
  html_attr("href")

#linkleri temizleyerek tak??m isimlerini elde edece??iz
takimlar<-c()
#for dongusu ile her linki temizleyip en sonununda da takimlar de??i??kenine ekliyoruz
for (val in takim)
{
  a<-gregexpr(pattern ='/',val)
  val=substr(val, a[[1]][1], a[[1]][2])
  val<-sub("/","", val)
  val<-sub("/","", val)
  val<-sub("-"," ", val)
  takimlar<-c(takimlar,val)
  
}
takimlar<-as.factor(takimlar)

#tum bilgileri oyuncu_Data ad??nda data framee koyuyoruz
oyuncu_Data<-data.frame(oyuncu_isim,pozisyon,takimlar,yas,degisim)

#tak??m ve pozisyona gore gruplay??p tak??mlardaki pozisyona gore ortalama de??i??imi goruyoruz
oyuncu_Data%>%
  group_by(takimlar,pozisyon)%>%
  summarise(ort=mean(degisim))

#sadece tak??ma gore gruplay??p,tak??m baz??nda degi??imi goruyoruz
oyuncu_Data%>%
  group_by(takimlar)%>%
  summarise(ortalama_degisim=mean(degisim))%>%
  ggplot()+
  geom_bar(aes(x=takimlar,y=ortalama_degisim),stat = "identity")+
  coord_flip()





a<-MyData%>%
  group_by(MyData$Pozisyon)%>%
  ggplot(.)+
  geom_bar(aes(x=MyData$Pozisyon))



ggplot(MyData)+
  geom_point(aes(x=Ya??, y=De??i??im))


ggplot(my) +
  geom_bar(aes(x=MyData$Pozisyon))











