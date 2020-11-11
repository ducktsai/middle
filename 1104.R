#房屋購買數量分布
install.packages("dplyr")
library(dplyr)
library(jsonlite)
library(lattice)

setwd("C:/mycode/1104middle")
house='houseKH.json'#json是用UTF-8內碼編碼,不會有csv中文讀取失敗的問題
download.file('https://quality.data.gov.tw/dq_download_json.php?nid=104268&md5_url=13b1c3169cc7c18d6d5855e9df306b10',house)
jhdata <- fromJSON("houseKH.json")
numjhdata<-as.data.frame(jhdata)
#
Area<-table(jhdata$行政區)
Area1<-sort(Area,decreasing = T)
Deals<-as.data.frame(Area1)
names(Deals)<-c('地區','數量')
#stringsAsFactors預設將資料轉為Factor較省空間,記得設為F保存為dataframe
#鹽埕1
Dist1<-numjhdata%>%filter(行政區%in%"鹽埕區")
Type1<-table(Dist1$建物型態)
Yan<-sort(Type1,decreasing = F)
YanchengType=as.data.frame(Yan,stringsAsFactors=F)
colnames(YanchengType)<-c("類型","數量")
YanchengType[3,1] = "土地"
#鼓山2
Dist2<-numjhdata%>%filter(行政區%in%"鼓山區")
Type2<-table(Dist2$建物型態)
Gu<-sort(Type2,decreasing = F)
GushanType=as.data.frame(Gu,stringsAsFactors=F)
colnames(GushanType)<-c("類型","數量")
GushanType[6,1] = "土地"
#左營3
Dist3<-numjhdata%>%filter(行政區%in%"左營區")
Type3<-table(Dist3$建物型態)
Zuo<-sort(Type3,decreasing = F)
ZuoyingType=as.data.frame(Zuo,stringsAsFactors=F)
colnames(ZuoyingType)<-c("類型","數量")
ZuoyingType[8,1] = "土地"
#楠梓4
Dist4<-numjhdata%>%filter(行政區%in%"楠梓區")
Type4<-table(Dist4$建物型態)
Nan<-sort(Type4,decreasing = F)
NanziType=as.data.frame(Nan,stringsAsFactors=F)
colnames(NanziType)<-c("類型","數量")
NanziType[7,1] = "土地"
#三民5
Dist5<-numjhdata%>%filter(行政區%in%"三民區")
Type5<-table(Dist5$建物型態)
San<-sort(Type5,decreasing = F)
SanminType=as.data.frame(San,stringsAsFactors=F)
colnames(SanminType)<-c("類型","數量")
SanminType[4,1] = "土地"
#新興6
Dist6<-numjhdata%>%filter(行政區%in%"新興區")
Type6<-table(Dist6$建物型態)
Xin<-sort(Type6,decreasing = F)
XinxingType=as.data.frame(Xin,stringsAsFactors=F)
colnames(XinxingType)<-c("類型","數量")
XinxingType[3,1] = "土地"
#前金7
Dist7<-numjhdata%>%filter(行政區%in%"前金區")
Type7<-table(Dist7$建物型態)
Qian<-sort(Type7,decreasing = F)
QianjinType=as.data.frame(Qian,stringsAsFactors=F)
colnames(QianjinType)<-c("類型","數量")
QianjinType[8,1] = "土地"
#苓雅8
Dist8<-numjhdata%>%filter(行政區%in%"苓雅區")
Type8<-table(Dist8$建物型態)
Ling<-sort(Type8,decreasing = F)
LingyaType=as.data.frame(Ling,stringsAsFactors=F)
colnames(LingyaType)<-c("類型","數量")
LingyaType[3,1] = "土地"
#前鎮9
Dist9<-numjhdata%>%filter(行政區%in%"前鎮區")
Type9<-table(Dist9$建物型態)
zhen<-sort(Type9,decreasing = F)
QianzhenType=as.data.frame(zhen,stringsAsFactors=F)
colnames(QianzhenType)<-c("類型","數量")
QianzhenType[9,1] = "土地"
#旗津10
Dist10<-numjhdata%>%filter(行政區%in%"旗津區")
Type10<-table(Dist10$建物型態)
Qi<-sort(Type10,decreasing = F)
QijinType=as.data.frame(Qi,stringsAsFactors=F)
colnames(QijinType)<-c("類型","數量")
QijinType[2,1] = "土地"
#小港11Xiao
Dist11<-numjhdata%>%filter(行政區%in%"小港區")
Type11<-table(Dist11$建物型態)
Xiao<-sort(Type11,decreasing = F)
XiaogangType=as.data.frame(Xiao,stringsAsFactors=F)
colnames(XiaogangType)<-c("類型","數量")
XiaogangType[6,1] = "土地"
#鳳山12Feng
Dist12<-numjhdata%>%filter(行政區%in%"鳳山區")
Type12<-table(Dist12$建物型態)
Feng<-sort(Type12,decreasing = F)
FengshanType=as.data.frame(Feng,stringsAsFactors=F)
colnames(FengshanType)<-c("類型","數量")
FengshanType[5,1] = "土地"
#林園13yuan
Dist13<-numjhdata%>%filter(行政區%in%"林園區")
Type13<-table(Dist13$建物型態)
yuan<-sort(Type13,decreasing = F)
LinyuanType=as.data.frame(yuan,stringsAsFactors=F)
colnames(LinyuanType)<-c("類型","數量")
LinyuanType[7,1] = "土地"
#大寮14Da
Dist14<-numjhdata%>%filter(行政區%in%"大寮區")
Type14<-table(Dist14$建物型態)
Da<-sort(Type14,decreasing = F)
DaliaoType=as.data.frame(Da,stringsAsFactors=F)
colnames(DaliaoType)<-c("類型","數量")
DaliaoType[11,1] = "土地"
#大樹15shu
Dist15<-numjhdata%>%filter(行政區%in%"大樹區")
Type15<-table(Dist15$建物型態)
shu<-sort(Type15,decreasing = F)
DashuType=as.data.frame(shu,stringsAsFactors=F)
colnames(DashuType)<-c("類型","數量")
DashuType[7,1] = "土地"
#大社16she
Dist16<-numjhdata%>%filter(行政區%in%"大社區")
Type16<-table(Dist16$建物型態)
she<-sort(Type16,decreasing = F)
DasheType=as.data.frame(she,stringsAsFactors=F)
colnames(DasheType)<-c("類型","數量")
DasheType[7,1] = "土地"
#仁武17Ren
Dist17<-numjhdata%>%filter(行政區%in%"仁武區")
Type17<-table(Dist17$建物型態)
Ren<-sort(Type17,decreasing = F)
RenwuType=as.data.frame(Ren,stringsAsFactors=F)
colnames(RenwuType)<-c("類型","數量")
RenwuType[7,1] = "土地"
#鳥松18Niao
Dist18<-numjhdata%>%filter(行政區%in%"鳥松區")
Type18<-table(Dist18$建物型態)
Niao<-sort(Type18,decreasing = F)
NiaosongType=as.data.frame(Niao,stringsAsFactors=F)
colnames(NiaosongType)<-c("類型","數量")
NiaosongType[6,1] = "土地"
#岡山19Gang
Dist19<-numjhdata%>%filter(行政區%in%"岡山區")
Type19<-table(Dist19$建物型態)
Gang<-sort(Type19,decreasing = F)
GangshanType=as.data.frame(Gang,stringsAsFactors=F)
colnames(GangshanType)<-c("類型","數量")
GangshanType[9,1] = "土地"
#橋頭20Qiao
Dist20<-numjhdata%>%filter(行政區%in%"橋頭區")
Type20<-table(Dist20$建物型態)
Qiao<-sort(Type20,decreasing = F)
QiaotouType=as.data.frame(Qiao,stringsAsFactors=F)
colnames(QiaotouType)<-c("類型","數量")
QiaotouType[7,1] = "土地"
#燕巢21chao
Dist21<-numjhdata%>%filter(行政區%in%"燕巢區")
Type21<-table(Dist21$建物型態)
chao<-sort(Type21,decreasing = F)
YanchaoType=as.data.frame(chao,stringsAsFactors=F)
colnames(YanchaoType)<-c("類型","數量")
YanchaoType[5,1] = "土地"
#田寮22Tian
Dist22<-numjhdata%>%filter(行政區%in%"田寮區")
Type22<-table(Dist22$建物型態)
Tian<-sort(Type22,decreasing = F)
TianliaoType=as.data.frame(Tian,stringsAsFactors=F)
colnames(TianliaoType)<-c("數量")
rownames(TianliaoType)<-c("土地")
#阿蓮23Alian
Dist23<-numjhdata%>%filter(行政區%in%"阿蓮區")
Type23<-table(Dist23$建物型態)
Alian<-sort(Type23,decreasing = F)
AlianType=as.data.frame(Alian,stringsAsFactors=F)
colnames(AlianType)<-c("類型","數量")
AlianType[5,1] = "土地"
#路竹24Lu
Dist24<-numjhdata%>%filter(行政區%in%"路竹區")
Type24<-table(Dist24$建物型態)
Lu<-sort(Type24,decreasing = F)
LuzhuType=as.data.frame(Lu,stringsAsFactors=F)
colnames(LuzhuType)<-c("類型","數量")
LuzhuType[6,1] = "土地"
#湖內25
Dist25<-numjhdata%>%filter(行政區%in%"湖內區")
Type25<-table(Dist25$建物型態)
Hu<-sort(Type25,decreasing = F)
HuneiType=as.data.frame(Hu,stringsAsFactors=F)
colnames(HuneiType)<-c("類型","數量")
HuneiType[9,1] = "土地"
#茄萣26Qieding
Dist26<-numjhdata%>%filter(行政區%in%"茄萣區")
Type26<-table(Dist26$建物型態)
Qie<-sort(Type26,decreasing = F)
QiedingType=as.data.frame(Qie,stringsAsFactors=F)
colnames(QiedingType)<-c("類型","數量")
QiedingType[3,1] = "土地"
#永安27Yongan
Dist27<-numjhdata%>%filter(行政區%in%"永安區")
Type27<-table(Dist27$建物型態)
Yong<-sort(Type27,decreasing = F)
YonganType=as.data.frame(Yong,stringsAsFactors=F)
colnames(YonganType)<-c("類型","數量")
YonganType[4,1] = "土地"
#彌陀28Mituo
Dist28<-numjhdata%>%filter(行政區%in%"彌陀區")
Type28<-table(Dist28$建物型態)
Mi<-sort(Type28,decreasing = F)
MituoType=as.data.frame(Mi,stringsAsFactors=F)
colnames(MituoType)<-c("類型","數量")
MituoType[3,1] = "土地"
#梓官29Zi
Dist29<-numjhdata%>%filter(行政區%in%"梓官區")
Type29<-table(Dist29$建物型態)
Zi<-sort(Type29,decreasing = F)
ZiguanType=as.data.frame(Zi,stringsAsFactors=F)
colnames(ZiguanType)<-c("類型","數量")
ZiguanType[6,1] = "土地"
#旗山30Qishan
Dist30<-numjhdata%>%filter(行政區%in%"旗山區")
Type30<-table(Dist30$建物型態)
Qishan<-sort(Type30,decreasing = F)
QishanType=as.data.frame(Qishan,stringsAsFactors=F)
colnames(QishanType)<-c("類型","數量")
QishanType[5,1] = "土地"
#美濃31Mei
Dist31<-numjhdata%>%filter(行政區%in%"美濃區")
Type31<-table(Dist31$建物型態)
Mei<-sort(Type31,decreasing = F)
MeinongType=as.data.frame(Mei,stringsAsFactors=F)
colnames(MeinongType)<-c("類型","數量")
MeinongType[4,1] = "土地"
#六龜32Liugui
Dist32<-numjhdata%>%filter(行政區%in%"六龜區")
Type32<-table(Dist32$建物型態)
Liu<-sort(Type32,decreasing = F)
LiuguiType=as.data.frame(Liu,stringsAsFactors=F)
colnames(LiuguiType)<-c("類型","數量")
LiuguiType[3,1] = "土地"
#甲仙33Jiaxian
Dist33<-numjhdata%>%filter(行政區%in%"甲仙區")
Type33<-table(Dist33$建物型態)
Jia<-sort(Type33,decreasing = F)
JiaxianType=as.data.frame(Jia,stringsAsFactors=F)
colnames(JiaxianType)<-c("類型","數量")
JiaxianType[2,1] = "土地"
#杉林34Shan
Dist34<-numjhdata%>%filter(行政區%in%"杉林區")
Type34<-table(Dist34$建物型態)
Shan<-sort(Type34,decreasing = F)
ShanlinType=as.data.frame(Shan,stringsAsFactors=F)
colnames(ShanlinType)<-c("類型","數量")
ShanlinType[2,1] = "土地"
#內門35Nei
Dist35<-numjhdata%>%filter(行政區%in%"內門區")
Type35<-table(Dist35$建物型態)
Nei<-sort(Type35,decreasing = F)
NeimenType=as.data.frame(Nei,stringsAsFactors=F)
colnames(NeimenType)<-c("數量")
rownames(NeimenType)<-c("土地")
#茂林36Mao
Dist36<-numjhdata%>%filter(行政區%in%"茂林區")
Type36<-table(Dist36$建物型態)
Mao<-sort(Type36,decreasing = F)
MaolinType=as.data.frame(Mao,stringsAsFactors=F)
colnames(MaolinType)<-c("數量")
rownames(MaolinType)<-c("土地")
#桃源37Tao
Dist37<-numjhdata%>%filter(行政區%in%"桃源區")
Type37<-table(Dist37$建物型態)
Tao<-sort(Type37,decreasing = F)
TaoyuanType=as.data.frame(Tao,stringsAsFactors=F)
colnames(TaoyuanType)<-c("數量")
rownames(TaoyuanType)<-c("土地")
#那瑪夏38Nama
Dist38<-numjhdata%>%filter(行政區%in%"那瑪夏區")
Type38<-table(Dist38$建物型態)
Nama<-sort(Type38,decreasing = F)
NamaxiaType=as.data.frame(Nama,stringsAsFactors=F)
colnames(NamaxiaType)<-c("數量")
rownames(NamaxiaType)<-c("土地")
#盒鬚圖房地價
numjhdata$房地總價<-as.numeric(numjhdata$房地總價)
P1<-group_by(numjhdata,行政區)%>%summarise((sum(across(房地總價,sum)))/(10^8))
colnames(P1)<-c("行政區","房地總價")
price<-arrange(P1,desc(房地總價))
boxplot(房地總價~行政區,numjhdata,xlab="行政區",ylab="房地總價",main="各地區房價")
boxplot(房地總價~行政區,Dist35,xlab="內門區",ylab="房地總價",main="房價")

