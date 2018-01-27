#Update : 2018-01-27, Kang Han.

#Reference : http://prohannah.tistory.com/30

#install.packages('XML')
#install.packages('stringr')
#install.packages('tidyverse')

library('XML')
library('stringr')
library('tidyverse')

url_base <- "http://www.catholicnews.co.kr/news/articleList.html?page="

all.number <- c() #기사 번호 목록
all.datetime <- c() #연월일시분 목록
all.title <- c() #기사 제목 목록
all.name <- c() #작성자 이름 목록

#기사 목록의 마지막 페이지 번호를 확인할 것.
#수정된 Code로 전체 기사 목록 수집.
for(i in 1:887)
{
  url <- paste(url_base, i, sep='')
  txt <- readLines(url, encoding="euc-kr")
  
  titleline <- txt[which(str_detect(txt, "list-titles"))]
  dateline <- txt[which(str_detect(txt, "list-times"))]
  nameline <- txt[which(str_detect(txt, "list-name"))]
  
  number <- gsub("\\D", "", str_sub(titleline, 84, 88))
  datetime <- str_sub(gsub("\\D", "", dateline), 4, 15)
  title <- gsub("<.+?>|\t", "", titleline)
  name <- gsub("<.+?>|\t", "", nameline)
  
  all.number <- c(all.number, number) #기사 번호 저장
  all.datetime <- c(all.datetime, datetime) #연월일 저장
  all.name <- c(all.name, name) #작성자 이름 저장
  all.title <- c(all.title, title) #주제목 저장
}

newslist <- cbind(all.number, all.datetime, all.name, all.title) #1개 Matrix로 조합

newslist <- as_tibble(newslist) #tibble로 변환

#기사 번호 교정
#아래 [] 안의 숫자는 새로운 기사가 추가됨에 따라 더 커지기 때문에
#매번 스크랩 때마다 확인하고 조정해야 한다.
newslist$all.number[17687] <- "56"
newslist$all.number[17661] <- "82"
newslist$all.number[17660] <- "83"
newslist$all.number[17659] <- "84"


#확인 및 데이터 교정(잘못된 띄어쓰기 수정 등) 작업.
newslist$all.name <- str_replace(newslist$all.name, " 기자", "")
newslist$all.name <- str_replace(newslist$all.name, " 수습기자", "")
newslist$all.name <- str_replace(newslist$all.name, "기자", "")
newslist$all.name <- str_replace(newslist$all.name, "  ", " ")
newslist$all.name <- str_replace(newslist$all.name, "한상봉 ", "한상봉")
newslist$all.name <- str_replace(newslist$all.name, "한상봉주필", "한상봉")
newslist$all.name <- str_replace(newslist$all.name, "한상봉편집장", "한상봉")
newslist$all.name <- str_replace(newslist$all.name, " 객원", "")
newslist$all.name <- str_replace(newslist$all.name, " 대주교", "")
newslist$all.name <- str_replace(newslist$all.name, " 주교", "")
newslist$all.name <- str_replace(newslist$all.name, " 신부", "")
newslist$all.name <- str_replace(newslist$all.name, " 천주교인권위원회 사무국장", "")
newslist$all.name <- str_replace(newslist$all.name, "뉴스 지금여기", "가톨릭뉴스 지금여기")
newslist$all.name <- str_replace(newslist$all.name, "가톨릭가톨릭뉴스 지금여기", "가톨릭뉴스 지금여기")
newslist$all.name <- str_replace(newslist$all.name, " 아곱 수녀", "")
newslist$all.name <- str_replace(newslist$all.name, " 통신원", "")
newslist$all.name <- str_replace(newslist$all.name, " 예비객원", "")
newslist$all.name <- str_replace(newslist$all.name, " 예비", "")
newslist$all.name <- str_replace(newslist$all.name, " 통신원", "")
newslist$all.name <- str_replace(newslist$all.name, "리차드 로어", "리처드 로어")
newslist$all.name <- str_replace(newslist$all.name, "윌리언 그림", "윌리엄 그림")
newslist$all.name <- str_replace(newslist$all.name, " 수녀", "")
newslist$all.name <- str_replace(newslist$all.name, " 수사", "")
newslist$all.name <- str_replace(newslist$all.name, " 명예", "")
newslist$all.name <- str_replace(newslist$all.name, " 스님", "")
newslist$all.name <- str_replace(newslist$all.name, " 시인", "")
newslist$all.name <- str_replace(newslist$all.name, "(서강대 법학전문대학원)", "")
newslist$all.name <- str_replace(newslist$all.name, "천주교인권위", "천주교인권위원회")
newslist$all.name <- str_replace(newslist$all.name, "천주교정의구현사제단", "천주교정의구현전국사제단")

write_csv(newslist, "newslist.csv")

rm(list = ls())

library('hms')

newslist <- read_csv("newslist.csv")

newslist$date <- str_sub(newslist$all.datetime, 1, 8)
newslist$time <- str_sub(newslist$all.datetime, 9, 12)

newslist$date <- parse_date(newslist$date, "%Y%m%d")
newslist$time <- parse_time(newslist$time, "%H%M")

newslist_m <- newslist %>% 
  select(all.number, date, time, all.name, all.title)

newslist_m <- rename(newslist_m, NUMBER = all.number)
newslist_m <- rename(newslist_m, DATE = date)
newslist_m <- rename(newslist_m, TIME = time)
newslist_m <- rename(newslist_m, NAME = all.name)
newslist_m <- rename(newslist_m, TITLE = all.title)

write_csv(newslist_m, "newslist_m.csv")

rm(list = ls())

newslist <- read_csv("newslist_m.csv")

#편집국 자체 생산 기사와 외부 기고 등을 구분하기 위한 작업.
newslist$TYPE <- ifelse(newslist$NAME == "가톨릭뉴스 지금여기", 1,
                        ifelse(newslist$NAME == "강한", 1,
                               ifelse(newslist$NAME == "강한 · 정현진 · 문양효숙", 1,
                                      ifelse(newslist$NAME == "강한 · 한수진", 1,
                                             ifelse(newslist$NAME == "강한, 배선영", 1,
                                                    ifelse(newslist$NAME == "강한, 정현진", 1,
                                                           ifelse(newslist$NAME == "강한, 정호준", 1,
                                                                  ifelse(newslist$NAME == "고동주", 1,
                                                                         ifelse(newslist$NAME == "고동주, 김용길(사진)", 1,
                                                                                ifelse(newslist$NAME == "고동주, 배은주", 1,
                                                                                       ifelse(newslist$NAME == "고동주/ 사진 김용길", 1,
                                                                                              ifelse(newslist$NAME == "고동주/박오늘", 1,
                                                                                                     ifelse(newslist$NAME == "고동주/배은주", 1,
                                                                                                            ifelse(newslist$NAME == "글 고동주, 사진 김용길", 1,
                                                                                                                   ifelse(newslist$NAME == "글 고동주/사진 김용길", 1,
                                                                                                                          ifelse(newslist$NAME == "글 고동주/사진 배은주", 1,
                                                                                                                                 ifelse(newslist$NAME == "김용길, 강한", 1,
                                                                                                                                        ifelse(newslist$NAME == "문양효숙", 1,
                                                                                                                                               ifelse(newslist$NAME == "배선영", 1,
                                                                                                                                                      ifelse(newslist$NAME == "배선영, 강한", 1,
                                                                                                                                                             ifelse(newslist$NAME == "배선영, 왕기리", 1,
                                                                                                                                                                    ifelse(newslist$NAME == "배선영, 정현진", 1,
                                                                                                                                                                           ifelse(newslist$NAME == "배은주 고동주", 1,
                                                                                                                                                                                  ifelse(newslist$NAME == "왕기리", 1,
                                                                                                                                                                                         ifelse(newslist$NAME == "정현진", 1,
                                                                                                                                                                                                ifelse(newslist$NAME == "정현진, 강한", 1,
                                                                                                                                                                                                       ifelse(newslist$NAME == "정현진, 배선영", 1,
                                                                                                                                                                                                              ifelse(newslist$NAME == "정호준", 1,
                                                                                                                                                                                                                     ifelse(newslist$NAME == "정호준, 강한", 1,
                                                                                                                                                                                                                            ifelse(newslist$NAME == "조지혜", 1,
                                                                                                                                                                                                                                   ifelse(newslist$NAME == "지금여기 취재팀", 1,
                                                                                                                                                                                                                                          ifelse(newslist$NAME == "편집국", 1,
                                                                                                                                                                                                                                                 ifelse(newslist$NAME == "한상봉", 1,
                                                                                                                                                                                                                                                        ifelse(newslist$NAME == "한상봉(사진/김용길)", 1,
                                                                                                                                                                                                                                                               ifelse(newslist$NAME == "한상봉(사진: 김용길)", 1,
                                                                                                                                                                                                                                                                      ifelse(newslist$NAME == "한상봉, 고동주", 1,
                                                                                                                                                                                                                                                                             ifelse(newslist$NAME == "한상봉, 사진/김용길", 1,
                                                                                                                                                                                                                                                                                    ifelse(newslist$NAME == "한수진", 1, 0))))))))))))))))))))))))))))))))))))))

#TYPE 컬럼을 Factor로 변환.
newslist$TYPE <- factor(newslist$TYPE, labels=c("칼럼 및 기타", "편집국 자체 생산"))

#월별 추출(2017년 12월의 예)
newslist201712 <- newslist %>% 
  filter(DATE >= "2017-12-01" & DATE <= "2017-12-31")

#월별 통계 자료 만들기
stat201712 <- newslist201712 %>% 
  group_by(TYPE) %>% 
  summarise(
    COUNT = n(),
    PROPORTION = (n() / dim(newslist201712)[1])*100
  )

#Excel 사용하는 동료들을 위해(Encoding 문제) write_excel_csv로 저장.
write_excel_csv(stat201712, "stat201712.csv")