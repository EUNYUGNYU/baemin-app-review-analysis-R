# 패키지 불러오기
library(readr) # SentiWord_Dict.txt을 불러오는 패키지
library(readxl) # 엑셀 파일을 불러오는 패키지
library(dplyr) 
library(stringr) # 전처리시 사용하는 패키지
library(textclean) #전처리(HTML 특수 문자 제거)시 사용하는 패키지
library(tidytext) # 명사 추출시 사용하는 패키지
library(KoNLP) # 명사 추출시 사용하는 패키지
library(scales) # 그래프 축 맞출 때 사용되는 패키지
library(ggplot2) # 그래프 그릴 때 사용되는 패키지
library(tidyr) #log RR 구할 때 사용되는 패키지
library(ldatuning) # 최적 lda 모델을 찾을 때 사용하는 패키지
library(tidylo) # 가중 로그 오즈비를 구하는 패키지
library(ggwordcloud) # 워드 클라우드 그리는 패키지
library(showtext)  # 외부 글꼴 추가 패키지
library(gridExtra) # 워드클라우드 동시에 불러오는 패키지
font_add_google(name = "Black Han Sans", family = "blackhansans") # '검은고딕' 폰트 적용
showtext_auto()


# 1. 배달의 민족을 읽어 들여 텍스트 분석을 위한 전처리 하는 과정을 보이고 설명하시오. (10)  

# 데이터 불러오기
DF <- read_excel("배달의 민족.xlsx") %>% 
  mutate(id = row_number()) # document를 편하게 구분하기 위해 id번호를 생성.

# 데이터 간단 탐색
head(DF,10)
glimpse(DF)

baemin <- DF %>%
  mutate(reply = str_replace_all(리뷰내용, "[^가-힣]", " "),
         reply = str_squish(reply)) %>%  #닉네임별로 중복되는 댓글이 없으니 중복 댓글 제거할 필요 없음
  filter(str_count(reply, pattern=boundary(type= "word")) >= 2)  # 짧은 문서 제거 :  2 단어 이상 추출 (Topic modeling은 짧은 문서는 적합하지 않음)

# 명사 추출 (토큰화) : 댓글 내 중복 단어를 제거하지 않음.
comment <- baemin %>%
  unnest_tokens(input = reply, 
                output = word,
                token = extractNoun,
                drop = F) %>% 
  filter(str_count(word) > 1) # 단어가 1번 나오는 경우는 (n=1) 제외. 최소 2번 나와야 함.

comment<-comment %>%
  select(id, word) %>% print()

# 댓글의 주제어 혹은 모든 댓글에 빈도 높은 단어 제거하기 => "배민" 삭제

# 댓글의 주제어 "배민" 삭제
count_word <- comment %>%
  add_count(word, sort=TRUE) %>% # mutate성의 개별 결과 제공
  filter(!word %in% c("배민","배달의민족")) %>% print() # "배달" 단어는 배달 서비스 자체에 대한 언급시 사용될 수 있으므로 유지. 앱 이름과 동일한 "배민" 만 삭제함

count_word %>% 
  count(word) %>% 
  filter(word=="민족")

count_word %>% 
  count(word)
# 불용어 및 유의어 처리
count_word %>%
  count(word, sort=TRUE) %>% View() # stopword 처리를 위해 View 함수 사용

# stopword 제거 (기준 : 상위 200개 단어)

stopword <- c("들이", "하다", "하게", "하면", "해서", "이번", "하네",
              "해요", "이것", "니들", "하기", "하지", "한거", "해주",
              "그것", "어디", "여기", "까지", "이거", "하신", "만큼",
              "진짜", "하려", "하시","하라", "한거", "해도")

count_word <- count_word %>%
  filter(!word %in% stopword) %>%
  # 유의어 처리
  mutate(word = dplyr::recode(word,
                              "업뎃" = "업데이트",
                              "독점" = "독과점")) # 해당 원문에서는 독점과 독과점이 거의 유사한 뜻으로 사용됨


# 2. 전체 댓글들에 대한 워드클라우드를 그리고 설명을 하시오. (10)

# 단어별 사용 빈도 (add_count에서 반복 제거)
word_cloud <-count_word %>% 
  count(word, sort=T) %>% print()

# 워드 클라우드 그리기
word_cloud %>% 
  ggplot(aes(label = word, size = n, 
             color = factor(sample.int(n=10, 
                                       size=nrow(word_cloud), replace = TRUE)))) + # 색깔 지정
  geom_text_wordcloud(seed = 1234, family = "blackhansans",size=30, face = "bold") + # 폰트 적용
  scale_radius(limits = c(30, NA), # 최소, 최대 단어 빈도 (최소 30번 이상 나온 단어들이 나온다.)
               range = c(3, 27))+  # 최소, 최대 글자 크기
  ggtitle("전체 댓글의 WORD CLOUD") +
  theme_minimal() 

# => 최소 30번 이상 나온 단어들을 가지고 워드 클라우드를 그려보았다. 워드 클라우드는 '어떤 단어를 많이 사용했는가? (빈도) '에 대해 한 눈에 알 수 있게 나타낸 그래프이다. 현재 그래프에서는 가장 많이 사용된 '배달'이고 그 다음이 '주문', '수수료' 순으로 나타난다. 그 외에도 결제, 리뷰, 삭제, 이벤트, 업데이트, 사용, 어플 등 주로 배달의 민족 APP 에 대한 단어가 주로 나오는 것으로 확인된다. 
# 좀 더 보충 설명하기 



# 3. 토픽을 찾기 위한 적합한 토픽의 수를 찾고 설명하기 바랍니다. (10)


### 적합한 토픽의 수를 찾는 방법을 하이퍼 파라미터 튜닝, 초모수 튜닝이라고 한다.

##### 이 방법은 토픽 수를 바꾸어 가며 여러 모델을 만든 후 모델별 성능을 비교한 것이다.

##### 토픽 수가 너무 적으면 대부분의 단어가 한 토픽의 주요 단어가 되므로, 토픽 모델링을 하지 않고 원데이터를 분석하는 것과 비슷해진다.

##### 토픽 수가 너무 많으면 여러 토픽에 주요 단어가 중복되어 토픽마다의 개성이 드러나지 않게 된다.

##### 따라서 최적의 토픽 수를 찾는 것이 중요하다.


# 문서별 단어 빈도 구하기
count_word_doc<- count_word %>%
  count(id, word, sort = T)

count_word_doc

# DTM (Documetn Term Matrix) 만들기
dtm_baemin <- count_word_doc %>%
  cast_dtm(document = id, term = word, value = n)

dtm_baemin

# dtm의 내용 확인하기 (15X15 행렬)
as.matrix(dtm_tada[1:15, 1:15])

##### 20개의 LDA 모델을 일단 만들어준다.

##### 각 LDA 모델에 나오는 Griffiths2004 는 복잡도이다. 모델의 성능 지표가 되며, 텍스트를 얼마나 잘 설명하는지를 표현한다. 텍스트의 구조를 잘 설명할 수록 값이 커지게 된다.


# 토픽 수 바꿔가면서 LDA 모델 여러개 만들기
models_baemin<- FindTopicsNumber(dtm = dtm_baemin,
                                topics = 2:20, # 20개의 LDA 모델을 만듦.
                                return_models = T,
                                control = list(seed = 1234))
models_baemin %>% 
  select(topics, Griffiths2004) #Griffiths2004 복잡도 (모델의 성능 지표, 텍스트를 설명하는 정도로 텍스트의 구조를 잘 설명할 수록 값이 크다.) 사용

##### 그래프를 그리면 최적 토픽 수에 대해 쉽게 파악할 수 있다.

##### 
# 성능 지표 그래프를 통해 최적 토픽 수 정하기 : 성능이 높을 수록 1에 가까움. 토픽 수를 늘려도 더 이상 성능이 크게 향상되지 않고 등락 반복하기 시작하는 지점에서 토픽 수를 선택함.
FindTopicsNumber_plot(models_baemin)
# => 토픽의 수는 7개 적합해 보임.

# 토픽 수가 7개인 모델 추출하기
optimal_model<-models_baemin %>%  
  filter(topics == 7) %>%
  pull(LDA_model) %>% # model 추출
  .[[1]] # list 추출

# 최적 모델 내용 확인
glimpse(optimal_model)


# 4. 찾은 각각의 토픽에 대해서 TF-IDF를구하고 각 토픽들의 중요단어 10개에 대한 막대그래프를 그리고 설명하시오. (10)     
# 감마 (문서가 각 토픽에 등장할 확률)를 사용하면 문서를 토픽별로 구분 가능

# gamma (문서가 각 토픽에 등장할 확률) 추출 
doc_topic<-tidy(optimal_model, matrix = "gamma")
doc_topic

# 감마가 동점이 발생한 경우 확인하기
doc_topic %>%
  group_by(document) %>%
  top_n(n=1, wt=gamma) %>% 
  count(document) %>% 
  filter(n > 1) %>% print()

# 문서별 확률이 가장 높은 토픽 추출
doc_class <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) 
doc_class # 감마가 동점이 발생하는 경우 (document가 여러 토픽으로 분류될 수 있다는 의미) 있음. 

# documnet 변수 타입을 정수형으로 통일 (데이터셋 결합하기 위해)
doc_class$document <- as.integer(doc_class$document) #데이터셋 변환 가능

# 원문에 감마 (확률) 이 가장 높게 나온 토픽의 번호 부여
baemin_topic <- baemin %>%
  left_join(doc_class, by = c("id" = "document"))

# 결합 확인 (기존 원문에 topic, gamma 변수와 값이 추가됨)
baemin_topic %>%
  select(id, topic)

# 토픽별 문서 수 보기
baemin_topic %>%
  count(topic) #topic이 NA 인 문서가 13개 있음.

# topic이 NA인 문서 제거
baemin_topic <- baemin_topic %>%
  na.omit()

baemin_topic %>%
  count(topic)

# => 원문에 토픽번호 붙인 후, 정리 완료


# 토픽번호가 붙은 원문 전처리 및 토큰화 진행
baemin_topic_token<-baemin_topic %>% 
  unnest_tokens(input = reply,
                output = word,
                token = extractNoun,
                drop = F) %>% 
  filter(str_count(word) > 1) # 단어가 1번 나오는 경우는 (n=1) 제외. 최소 2번 나와야 함.

# TF-IDF는 TOPIC의 개성을 드러내는 중요한 단어 (흔하지 않고 특정 TOPIC에서 자주 사용된 단어) 를 찾아주는 것이니 따로 주제어를 제거하지 않는다.
# TF-IDF는 TOPIC의 개성을 드러내는 중요한 단어 (흔하지 않고 특정 TOPIC에서 자주 사용된 단어) 를 찾아주는 것이니 STOPWORD를 제거하지 않는다.

# 단어 빈도 구하기
freq <-baemin_topic_token %>%
  count(topic, word) %>%  print()

# tf-idf 구하기
freq <- freq %>% 
  bind_tf_idf(term = word, # 단어는 word 변수, 
              document = topic, # 텍스트 구분 기준은 topic,
              n = n) %>% # 단어 빈도
  arrange(-tf_idf)
freq

# 토픽별 주요 단어 10개 추출
top10_tf_idf<- freq%>%
  group_by(topic) %>%
  slice_max(tf_idf, n = 10, with_ties = F)

# 그래프 순서 지정
top10_tf_idf$topic <- factor(top10_tf_idf$topic,
                         levels = c(1:7)) # 토픽 1->7 순으로 지정


# 막대 그래프 그리기 : 크기, 색 조정 필요
top10_tf_idf %>% 
  ggplot(aes(x = reorder_within(x=word, by=tf_idf, within=topic),
             y = tf_idf,
             fill = topic)) +
  labs(x = "", y = "y는 tf-idf", title = "TOPIC별 중요한 단어", subtitle="(다른 TOPIC 대비 강조한 단어)") +
  geom_text(aes(label = round(tf_idf,3) , hjust = -0.1),
            color = "dark grey", size=2)+ 
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~ topic, scales = "free", ncol = 4) +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(size=10))


# 5. 각각의 토픽들에 대해서 중요단어를 나타내는 beta의 값이 큰 순서대로 워드클라우드를 그리고 설명을 하시오. (10)
# 베타: 단어가 각 토픽에 들어갈 확률로 베타를 통해 각 토픽에 등장할 가능성이 높은 주요 단어를 알 수 있음.

# beta (단어가 각 토픽에 들어갈 확률) 추출하기
term_topic <- tidy(optimal_model, matrix = "beta") 
term_topic

# 모든 토픽의 주요 단어 한번에 보기
terms(optimal_model, 20) %>% 
  data.frame()

# 워드클라우드 그리기
#토픽 1의 워드 클라우드
term_topic1 <- term_topic %>% filter(topic == 1)
word1<-term_topic1 %>%
  ggplot(aes(label = term, size = beta*10000, # 크기는 beta*10000
             color = factor(sample.int(n=10,
                                       size=nrow(term_topic1), replace = TRUE)))) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(10, NA), # 최소 단어가 10번 이상 나와야 함.
               range = c(3,15)) +
  ggtitle("TOPIC 1")+
  theme_minimal()

word1

#토픽 2의 워드 클라우드
term_topic2 <- term_topic %>% filter(topic == 2)
word2<-term_topic2 %>%
  ggplot(aes(label = term, size = beta*10000, # 크기는 beta*10000
             color = factor(sample.int(n=10,
                                       size=nrow(term_topic2), replace = TRUE)))) + 
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(10, NA),  # 최소 단어가 10번 이상 나와야 함.
               range = c(3, 15)) +
  ggtitle("TOPIC 2")+
  theme_minimal() 

word2

#토픽 3의 워드 클라우드
term_topic3<- term_topic %>% filter(topic ==3)
word3<-term_topic3 %>%
  ggplot(aes(label = term, size = beta*10000, # 크기는 beta*10000
             color = factor(sample.int(n=10,
                                       size=nrow(term_topic3), replace = TRUE)))) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(10, NA),  # 최소 단어가 10번 이상 나와야 함.
               range = c(3, 15)) +
  ggtitle("TOPIC 3")+
  theme_minimal()

word3

#토픽 4의 워드 클라우드
term_topic4 <- term_topic %>% filter(topic == 4)
word4<-term_topic4 %>%
  ggplot(aes(label = term, size = beta*10000, # 크기는 beta*10000
             color = factor(sample.int(n=10,
                                       size=nrow(term_topic4), replace = TRUE)))) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(10, NA),  # 최소 단어가 10번 이상 나와야 함.
               range = c(3, 15)) +
  ggtitle("TOPIC 4")+
  theme_minimal()

word4

#토픽 5의 워드 클라우드
term_topic5 <- term_topic %>% filter(topic == 5)
word5<-term_topic5 %>%
  ggplot(aes(label = term, size = beta*10000, # 크기는 beta*10000
             color = factor(sample.int(n=10,
                                       size=nrow(term_topic5), replace = TRUE)))) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(10, NA),  # 최소 단어가 10번 이상 나와야 함.
               range = c(3, 15)) +
  ggtitle("TOPIC 5")+
  theme_minimal()

word5

#토픽 6의 워드 클라우드
term_topic6 <- term_topic %>% filter(topic == 6)
word6<-term_topic6 %>%
  ggplot(aes(label = term, size = beta*10000, # 크기는 beta*10000
             color = factor(sample.int(n=10,
                                       size=nrow(term_topic6), replace = TRUE)))) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(10, NA),  # 최소 단어가 10번 이상 나와야 함.
               range = c(3, 15)) +
  ggtitle("TOPIC 6")+
  theme_minimal()

word6

#토픽 7의 워드 클라우드
term_topic7 <- term_topic %>% filter(topic == 7)
word7<-term_topic7 %>%
  ggplot(aes(label = term, size = beta*10000, # 크기는 beta*10000
             color = factor(sample.int(n=10,
                                       size=nrow(term_topic7), replace = TRUE)))) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(10, NA), # 최소 단어가 10번 이상 나와야 함.
               range = c(3, 15)) +
  ggtitle("TOPIC 7")+
  theme_minimal()

word7

# 워드클라우드 여러개를 동시에 나타내기
grid.arrange(word1,word2,word3,word4,ncol=2)
grid.arrange(word5,word6,word7, ncol=2)


# 6. 각각의 토픽들에 대해서 중요단어를 나타내는 beta의 값의 크기에 따른 그래프를 그리고 설명하시오. (10)   


##### 5번에서 beta를 추출한 term_topic을 사용한다. 

##### beta에 대한 설명도 5번에서 진행했으므로 생략한다.


# 토픽별 beta가 높은 상위 10개 단어 추출
top10_term_topic<- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = F) %>% # 동점 제외
  ungroup() %>%
  arrange(topic, -beta)

top10_term_topic

# 막대 그래프로 나타내기
top10_term_topic %>% 
  ggplot(aes(x = reorder_within(term, beta, topic),
             y = beta,
             fill = factor(topic))) +
  labs(x = "", y = "y는 beta", title = "TOPIC별 중요한 단어", subtitle="(토픽별 각 토픽에 등장할 확률이 높은 단어)") +
  geom_text(aes(label = round(beta,3) , hjust = -0.2),
            color = "dark grey", size=2)+ 
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free",ncol = 4) +
  coord_flip() +
  scale_x_reordered() +
  theme(text = element_text(size=10))
  


# 7. 찾은 각각의 토픽의 집단 내에서의 감정분석을 하고 설명을 하시오. (10)

# 감정 사전 불러오기 (KNU 한국어 감성사전 사용)
senti_dic <- read_delim('C:/knu_senti_dict-master/knu_senti_dict-master/SentiWord_Dict.txt', delim='\t', col_names=c("word", "polarity"))

# 감정 사전 탐색
head(senti_dic,10)
dim(senti_dic)
table(senti_dic$polarity)

# 완전 긍정 (2점) 단어 보기
senti_dic %>%
  filter(polarity == 2) %>%
  arrange(word)

# 완전 부정 (-2점) 단어 보기
senti_dic %>%
  filter(polarity == -2) %>%
  arrange(word)

# 이모티콘 보기
senti_dic %>%
  filter(!str_detect(word, "[가-힣]")) %>%
  arrange(word)

# 감정사전 내 긍정 단어, 중립, 부정 단어의 수 보기
senti_dic %>%
  mutate(sentiment = ifelse(polarity >= 1, "pos",
                            ifelse(polarity <= -1, "neg", 
                                   "neu"))) %>%
  count(sentiment)

# 감정사전 내 완전 긍정 단어, 중립 (완전 긍정과 완전 부정이 아니면 모두 중립), 완전 부정 단어의 수 보기
senti_dic %>%
  mutate(sentiment = ifelse(polarity == 2, "pos",
                            ifelse(polarity == -2, "neg", 
                                   "neu"))) %>%
  count(sentiment)

# -----------------------

# topic 가 포함된 원문 token회 진행.
#(앞에서 진행한 토큰화는  형태소 분석 처리된 명사 형태 (baemin_topic_token의  word 변수)임. 이렇게 진행할 경우 동사가 누락될 수 있음. 따라서 words 기준으로 토큰화 진행.)

word_baemin<-baemin_topic_token %>%
  unnest_tokens(input = reply, 
                output = word, 
                token = "words", drop = F) %>%
  filter(str_count(word) > 1) %>% print() #최소 두 글자 이상의 단어만 사용


# 1-1. 토픽1의 최빈 단어별 감정분석 

# 토픽1 선택
topic1<-word_baemin %>% 
  filter(topic==1) %>% 
  select(topic, id, word, reply ) %>% print()

# 감정점수 부여
topic1 <- topic1 %>%
  left_join(senti_dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) # 사전에 없는 단어의 점수는 0

topic1 %>% 
  dplyr::select(word, polarity)

# 감정분류 : 감정이 확실한 완전긍정 (2점), 완전부정 (-2점) 단어만 사용한다.그 외 나머지는 모두 중립이다.
topic1 <- topic1 %>%
  mutate(sentiment = ifelse(polarity ==2, "pos",
                            ifelse(polarity == -2, "neg", "neu")))

topic1 %>%
  count(sentiment)

top10_sentiment1 <- topic1 %>%
  filter(sentiment != "neu") %>% # 중립 단어 제외
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% print(n=Inf) # 중복 허용

# 막대 그래프 그리기
top10_sentiment1 %>% 
  ggplot(aes(x = reorder(word, n),
             y = n,
             fill = sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.1,
            color = "dark grey", size=6) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  labs(x = NULL,y = "빈도수 (n)", title = "토픽 1 에서 자주 사용된 감정단어 ") +
  theme(text = element_text(size=20))+
  theme(legend.box.background = element_rect(fill = "gray"), 
        legend.box.margin = margin(3, 3, 3, 3))



# 1-2. 토픽1의 댓글별 감정 점수 구하기

# 댓글별 감정 점수 (polarity의 합으로 계산함. 댓글(id로 구분)별로 구분)
score_comment1 <- topic1 %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

# 긍정 점수가 높은 댓글보기 
score_comment1 %>%
  select(score, reply) %>% 
  arrange(-score)

# 부정 점수가 높은 댓글보기
score_comment1 %>%
  select(score, reply) %>% 
  arrange(score)

# 감정 점수 빈도 : 0점이 부여된 댓글이 가장 많고, 긍정과 부정의 양 극단으로 갈 수록 빈도가 감소함
score_comment1 %>%
  count(score)

# 감정 분류하기
score_comment1 <- score_comment1 %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))

# 감정 빈도 및 비율 구하기
freq_score1 <- score_comment1 %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100) %>% print()

# 막대 그래프 만들기
ggplot(freq_score1, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) +
  scale_x_discrete(limits = c("pos", "neu", "neg"))+ # 축 순서 정하기
  theme(legend.box.background = element_rect(fill = "pink"), 
        legend.box.margin = margin(3, 3, 3, 3))+
  labs(x = "감정 단어 유형", y = "빈도수 (n)", title = "TOPIC 1에서 사용된 감정 단어 유형")


# 비율 누적 막대그래프 만들기 - 감정비율 표현
# 더미 변수 생성
freq_score1$dummy <- 0

ggplot(freq_score1, aes(x = dummy, y = ratio, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = paste0(round(ratio, 1), "%")), # % 표시
            position = position_stack(vjust = 0.5)) + # 가운데에 텍스트 표시
  theme(axis.title.x = element_blank(), # x축 이름 삭제
        axis.text.x = element_blank(), # x축 값 삭제
        axis.ticks.x = element_blank())+ # x축 값 삭제
  theme(legend.box.background = element_rect(fill = "skyblue"), 
        legend.box.margin = margin(3, 3, 3, 3))+
  labs(y = "비율 (총 100%)", title = "TOPIC 1에서 사용된 감정 단어 유형-비율")




# 1-3. 토픽1의 감정 범주 (긍정 O/부정) 별 많이 나오는 단어 빈도 (logRR 사용)

# score_comment를 토큰화 진행
comment1 <- score_comment1 %>%
  unnest_tokens(input = reply,
                output = word,
                token = "words", # extractNoun이라고 지정할 경우 명사만 나옴. 동사도 나와야하므로 그냥 word를 해줌
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") & # 한글 추출
           str_count(word) >= 2) # 두 글자 이상 추출
head(comment1)

# 감정 및 단어별 빈도 구하기 
freq_word1 <- comment1 %>%
  filter(str_count(word) >= 2) %>% # 2번 이상 등장하는 단어만
  count(sentiment, word, sort = T) %>% print()


# 긍정으로 분리된 댓글 중 빈번하게 등장하는 단어
freq_word1 %>%
  filter(sentiment == "pos")

# 부정으로 분리된 댓글 중 빈번하게 등장하는 단어
freq_word1 %>%
  filter(sentiment == "neg")

# 로그 상대효율 (log RR) 계산하기
comment_wide1 <- freq_word1 %>%
  filter(sentiment != "neu") %>%
  pivot_wider(names_from = sentiment, # sentiment의 범주를 변수로 사용
              values_from = n,
              values_fill = list(n = 0)) %>% print() # 단어 없으면 0

# 로그 RR
comment_wide1 <- comment_wide1 %>%
  mutate(log_RR = log(((pos + 1) / (sum(pos + 1))) /
                        ((neg + 1) / (sum(neg + 1)))))
comment_wide1

# log RR 이 가장 큰 단어 10개 추출 
top10_topic1 <- comment_wide1 %>%
  group_by(sentiment = ifelse(log_RR > 0, "pos", "neg")) %>%
  slice_max(abs(log_RR), n = 10, with_ties = F) # 로그RR 동점 단어 제외
top10_topic1

#막대 그래프 그리기

ggplot(top10_topic1, aes(x = reorder(word, log_RR), y = log_RR, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(size=10))+
  theme(legend.box.background = element_rect(fill = "gray"), 
        legend.box.margin = margin(3, 3, 3, 3))+
  labs(x = "", y = "y는 log_RR", title = "긍정/부정 댓글에서 자주 사용된 단어", subtitle="TOPIC 1") 
  





# 8. 각 토픽들에 대해 가중로그오즈비에 의한 주요단어 10개들에 다한 막대그래프를 그리시오. (10)

# 가중 로그 오즈비 구하기 (앞에서 사용한 freq 사용)
freq_log <- freq %>% 
  bind_log_odds(set = topic, feature = word, n = n) %>%
  arrange(desc(log_odds_weighted))
freq_log

# 주요 10개 단어 추출
top10_log <- freq_log %>%
  group_by(topic) %>%
  slice_max(log_odds_weighted, n = 10, with_ties = F)

# 그래프 순서 지정
top10_log$topic <- factor(top10_log$topic,
                         levels = c(1:7))

# 그래프 그리기
top10_log %>% 
  ggplot(aes(x = reorder_within(x=word, by=log_odds_weighted, within=topic),
             y = log_odds_weighted,
             fill = topic)) +
  labs(x = "", y = "y는 log_odds_weighted", title = "TOPIC별 중요한 단어", subtitle="(타 TOPIC 대비 상대적으로 많이 나오는 단어)") +
  geom_text(aes(label = round(log_odds_weighted,3) , hjust = -0.1),
            color = "dark grey", size=2)+ 
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~ topic, scales = "free", ncol = 4) +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(size=10))



# 9. 위의 모든 것을 고려하여 각 토픽에 대한 이름을 짓고 설명하시오. (10)

# 토픽별 문서 수와 단어를 막대그래프로 나타내기
# 토픽별 주요 단어 목록 만들기 (토픽별 beta 값이 높은 상위 6개의 단어가 나옴)
top6_terms <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 6, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))
top6_terms


#토픽별 문서 빈도 구하기
count_topic <- baemin_topic %>%
  count(topic) %>% print()

# 문서 빈도에 주요 단어 결합
count_topic_word <- count_topic %>%
  left_join(top6_terms, by = "topic") %>%
  mutate(topic_name = paste("Topic", topic)) %>% print()


# 토픽 별 문서 수와 주요 단어를 막대 그래프로 나타내기
ggplot(count_topic_word,
       aes(x = reorder(topic_name, n),
           y = n,
           fill = topic_name)) +
  geom_col(show.legend = F) +
  coord_flip() +
  geom_text(aes(label = comma(n, accuracy = 1)),  # 문서 빈도 표시
            hjust = -0.2, 
            col = "dark grey") + # 막대 밖에 표시
  geom_text(aes(label = term), # 주요 단어 표시
            hjust = 1.03, # 이건 막대 안에 표시
            col = "white",
            size=3) + 
  scale_y_continuous(expand = c(0, 0),  # y축-막대 간격 줄이기
                     limits = c(0, 700)) +
  labs(title = "토픽별 주요 단어 및 댓글 빈도",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold"))


# 토픽별로 gamma가 높은 50개의 주요 문서 (댓글) 추출
reply_topic <- baemin_topic %>%
  group_by(topic) %>%
  slice_max(gamma, n = 50) %>% 
  select(topic, gamma, reply) %>% 
  arrange(topic,-gamma) %>% print()


# 토픽1 내용 살펴보기
reply_topic %>%
  filter(topic == 1) %>%
  pull(reply)
#=> 첫 번째 토픽은 '배달의 민족' 어플의 가장 기본적인 기능인 배달'이다. '배달','이용','업체','라이더', '지역' 등의 단어가 도출되었으며, 
# 이 중 가장 기본 서비스인 '배달' 이 가장 높은 가중치를 기록하였다. 라이더가 제품을 픽업 후 고객에게 전달하는 것을 뜻하는 기본적인 배달 (라이더의) 서비스와  배달 지역에 관한 의견이 도출되었다.

# 토픽2 내용 살펴보기
reply_topic %>%
  filter(topic == 2) %>%
  pull(reply)

# 두번째 토픽은 기업 윤리로 '수수료',독과점' 등의 단어와 함께 '삭제','탈퇴','실망' 등의 극단적인 부정적 감정을 표출하는 단어가 같이 표출되었다.  이 중 ‘수수료’ 키워드가 가장 높은 가중치를 기록하였다. 이 토픽은 2020년 4월 배달의 민족을 운영하는 ‘우아한 형제들’이 배달 매출의 5.8%를 수수료로 떼는 정률제를 도입하는 이슈와 관련되어 발생하
였다. 또한 독일 기업 딜리버리히어로가 ‘배달의 민족’, ‘요기
요’, ‘배달통’을 모두 소유하며 시장의 98%를 점유하게 된 상황
에서 자영업자에게 부담을 주는 수수료 정책은 사회적으로 반
감을 일으켰고 불매운동이 이어지자, 배달의 민족은 공식 사과
와 함께 수습에 나섰다. 이는 서비스를 제공하는 기업의 정책이 
기업 윤리를 위반한다고 판단될 때 기업에 치명적인 영향을 줄 
수 있음을 보여주고 있다. 

# 토픽3 내용 살펴보기 : 
reply_topic %>%
  filter(topic == 3) %>%
  pull(reply)

# 세번째 토픽은  ‘서비스 불만'이다. 


# 토픽4 내용 살펴보기 : 리뷰  등록 및 사진 등록 관련 
reply_topic %>%
  filter(topic == 4) %>%
  pull(reply)
#네번째 토픽은 ‘고객 리뷰’로 ‘리뷰’, ’작성’, ’사진’, ’가게’, ’ 고객’ 등의 단어가 추출되었다. 구매자는 리뷰를 작성함으로서 
공급자에게 의견을 전달하며, 또한 다른 구매자의 리뷰를 읽음
으로써 구매 의사결정에 필요한 정보를 획득한다. 온라인 쇼핑
몰을 대상으로 수행한 연구에서는 온라인 리뷰를 통해 제품에 
대한 태도와 경험을 공유하여 구매 의도와 재방문 의도에 영향
력이 미친다는 것을 확인한 바가 있다 + 불만

# 토픽5 내용 살펴보기: 안드로이드 업데이트 관련 불만
reply_topic %>%
  filter(topic == 5) %>%
  pull(reply)

# ***
baemin_topic_token %>% 
  filter(topic==5) %>% 
  count(날짜) %>% 
  arrange(-n)

# 토픽6 내용 살펴보기: 어플 오류관련 불만
reply_topic %>%
  filter(topic == 6) %>%
  pull(reply)

# 토픽7: 할인 쿠폰 및 이벤트 제시
reply_topic %>%
  filter(topic == 7) %>%
  pull(reply)

‘이벤트’로 ’이벤트’, ‘할인’, ’쿠폰’, ‘포인트’, ‘사용’ 등의 단어가 추출되었다. 플랫폼 서비스 업체는 사용자 혹
은 공급자에게 인센티브를 제공함으로써 플랫폼에 더 많은 고객
이 유입 또는 잔류할 수 있도록 마케팅 전략을 낸다[29]. 배달 애
플리케이션의 경우 이벤트는 플랫폼 서비스에서 제공하는 이벤
트와 공급자인 음식점에서 자체적으로 제공하는 이벤트가 있다



# 토픽 이름 목록 만들기
name_topic <- tibble(topic = 1:7,
                     name = c("1. 기본적인 배달 서비스 \n개선 필요",
                              "2. 수수료 정책은 \n불매 운동을 불러와",
                              "3. 배달시장 독식은 \n고객 이탈을 불러와",
                              "4. 작성 오류 없고 믿을만한 \n리뷰 작성이 진행되어야",
                              "5. 주문 접수부터 취소까지, \n음식점 자체 서비스 개선 필요",
                              "6. 이벤트를 열어도 안드로이드 기기에서만  \n주문 오류 발생?!",
                              "7. 로그인부터 결제까지, \n다양한 APP 사용 오류 속 대응도 엉망"))


name = c("1. 배달지역부터 배달 라이더까지 \n기본적인 딜리버리 서비스 개선 필요",
         "2. 수수료 정책은 기업의 횡포, \n소비자의 불매 운동을 불러와",
         "3. 배달시장 독식은 \n고객 이탈을 불러와"
         
         "4. 작성 과정에서 오류 없는, \n 믿을만한 리뷰 작성이 진행되어야",
         "5. 주문 접수부터 취소까지, \n음식점 자체 서비스 개선 필요",
         "6. 이벤트를 열어도 안드로이드 기기에서만  \n주문 오류 발생?!",
         "7. 로그인부터 결제까지, \n다양한 APP 사용 오류 속 대응도 엉망"))
# 토픽 이름 결합하기
top_term_topic_name <- top10_term_topic %>%
  left_join(name_topic,  by = "topic")
top_term_topic_name

# 막대 그래프 만들기
ggplot(top_term_topic_name,
       aes(x = reorder_within(term, beta, name),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ name, scales = "free", ncol = 3) +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "배달의 민족 고객 리뷰 토픽",
       subtitle = "토픽별 주요 단어 TOP 10",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(text = element_text(size=8),
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_blank(),   # x축 이름 삭제
        axis.ticks.x = element_blank())  # x축 눈금 삭제

10. 전체적으로 배달의 민족의 댓글에 대한 텍스트마이닝을 통해 제시하고자 하는 시사점을 설명하시오. (10)

ts_topic<-baemin_topic %>% 
  group_by(날짜) %>% 
  count(topic) %>% 
  arrange(-n)

ggplot(ts_topic, aes(x=날짜, y=n, colour=factor(topic)))+
  geom_line(size=1.0) +
  ggtitle("토픽별 리뷰 등록 날짜") +
  labs(x = "등록 날짜", y = "등록 리뷰수 (n)")+
  theme(plot.title=element_text(size=25))
  


[출처] # 97 ] R ggplot2(8) 산점도, 선(시계열) 그래프 geom_point, geom_line|작성자 지그드시
