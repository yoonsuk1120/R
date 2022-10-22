#데이터 불러오기
library(dplyr)

# 문재인
raw_moon <- readLines("speech_moon.txt",encoding = "UTF-8")
moon <- raw_moon %>% 
  as_tibble() %>% 
  mutate(president = "moon")

# 박근혜
raw_park <- readLines("speech_park.txt", encoding = "UTF-8")
park <- raw_park %>% 
  as_tibble() %>% 
  mutate(president = "park")


# 행으로 데이터 합치기
bind_speeches <- bind_rows(park,moon) %>% 
  select(president, value)
head(bind_speeches)
tail(bind_speeches)

# 집단별 단어 빈도 구하기
library(stringr)
speeches <- bind_speeches %>% 
  # 데이터프레임 구조가 아니여서 mutate로 처리
  # 정규식을 사용하여 특수문자 제거
  mutate(value = str_replace_all(value, "[^가-힣]"," "),
         value = str_squish(value))
speeches

# 토큰화
install.packages("tidytext")
library(tidytext)
# 형태소 분석
library(KoNLP)
speeches <- speeches %>% 
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
speeches

#하위집단별 단어 빈도 구하기
# 두 연설문의 단어 빈도 구하기
frequency <- speeches %>% 
  count(president,word) %>% 
  filter(str_count(word) > 1)
head(frequency)

# 자주 사용되는 단어 추출하기
# 연설문에 가장 많이 사용된 단어 추출하기
top10 <- frequency %>% 
  group_by(president) %>% 
  slice_max(n, n = 10)
top10

top10 %>% 
  filter(president == "park")

# 빈도 동점 단어 제외하고 추출하기
top10 <- frequency %>% 
  group_by(president) %>% 
  slice_max(n,n=10,with_ties = F)
top10

# 막대그래프 구하기
library(ggplot2)
ggplot(top10,aes(x = reorder(word,n),
                 y = n,
                 fill = president))+geom_col() + coord_flip() +
                # 대통령을 기준으로 그래프 나누기
                facet_wrap(~president)

# 그래프 별로 y축 설정하기
# reorder 하면 빈도순으로 정렬
ggplot(top10, aes(x = reorder(word,n),
                  y = n,
                  fill = president)) + geom_col()+coord_flip() + facet_wrap(~president, scales = "free_y")

# 특정 단어 제거하고 막대 그래프 그리기
top10 <- frequency %>% 
  filter(word != "국민") %>% 
  group_by(president) %>% 
  slice_max(n, n = 10, with_ties = F)
top10
ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) + geom_col()+coord_flip()+ facet_wrap(~president, scales = "free_y")
# 단어의 빈도가 전체를 기준으로 되어있어 그래프 정렬이 이루어지지 않음
# 대통령 별로 빈도수 정렬 reorder_within
# 축 설정하기
ggplot(top10, aes(x = reorder_within(word,n,president),
                  y = n,
                  fill = president)) + geom_col() + coord_flip() + facet_wrap(~president, scales = "free_y") + 
  # 일자리__moon에서 moon 제거, 폰트 추가
  scale_x_reordered() + labs(x=NULL)+theme(text = element_text(family = "nanumgothic"))

# 워드 클라우드
library(wordcloud)
library(RColorBrewer)

# 각 대통령 연설문의 top20
top20 <- frequency %>% 
  group_by(president) %>% 
  slice_max(n, n = 20)
top20

pal <- brewer.pal(8, "Paired")
wordcloud(words = top20$word,
          # 빈도
          freq = top20$n,
          # 최소 빈도 수
          min.freq = 2,
          # 표현할 단어 수
          max.words = 200,
          # 고빈도 가운데로F 랜덤으로T
          random.order = F,
          rot.per = 0.3,
          # 크기
          scale = c(5, 0.5),
          colors = pal)

