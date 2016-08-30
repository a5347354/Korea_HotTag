#製作JSON格式
id_func = function(ID,No){
  blogID = "%22blogId%22:"
  logNo = "%22logNo%22:"
  char_22 = "%22"
  str_json = ""
  for(i in 1:length(ID)){
    blog = paste0(blogID,char_22,ID[i],char_22)
    log = paste0(logNo,char_22,No[i],char_22)
    str_json = paste0(str_json,"{",blog,",",log,"},")
  }
  return(substr(str_json, 1, nchar(str_json)-1))
}

#爬蟲
# i = 1:第幾頁
for(i in 1:20){
  #先擷取LogID以及blogID
  #日期可空白
  start_date = "2016-07-30"
  end_date = "2016-08-30"
  res = paste0("http://section.blog.naver.com/sub/SearchBlog.nhn?type=post&option.keyword=%EA%B5%B0%EA%B2%83%EC%A7%88&term=&option.startDate=",
               start_date,
               "&option.endDate=",
               end_date,
               "&option.page.currentPage=",
               i,
               "&option.orderBy=sim")
  ID = read_html(res) %>% html_nodes(".vBlogId") %>% html_attr("value")
  No = read_html(res) %>% html_nodes(".vLogNo") %>% html_attr("value")
  Dates = read_html(res) %>% html_nodes(".date") %>% html_text() 
  #再向伺服器請求LogID及blogID的資料
  get_URL_JSON = paste0("http://section.blog.naver.com/TagSearchAsync.nhn?variables=[",id_func(ID,No),"]")
  korea_blog = fromJSON(get_URL_JSON)
  korea_blog$logNo = paste0("http://blog.naver.com/mingoesthere/",korea_blog$logNo)
  #處理時間資料後再倒回
  korea_blog$time = Dates %>% as.character.Date() %>% as.POSIXct(.,format="%Y.%m.%d. %H:%M")
  if(i==1){
    korea_blogs = korea_blog 
  }else{
    korea_blogs = rbind(korea_blogs,korea_blog)
  }
}

#Tag詞頻計算
library(jiebaR) 
source('https://raw.githubusercontent.com/a5347354/BigData_Analysis/master/Demo20160731/CNCorpus.R')
# s_corpus <- CNCorpus()
# control_list=list(wordLengths=c(2,Inf),tokenize=space_tokenizer)
# s_dtm <- DocumentTermMatrix(,control=control_list) 

for(i in 1:length(korea_blogs$tags)){
  if(i == 1){
    tags = unlist(korea_blogs$tags[i])
  }else{
    tags = c(tags,unlist(korea_blogs$tags[i]))
  }
}

#畫圖呈現
library(plotly)
tags_tb = sort(table(tags),decreasing = FALSE)
tags_tb = tags_tb[as.numeric(tags_tb)>1]
plot_ly( x = as.numeric(tags_tb), 
         y = names(tags_tb), 
         mode = "markers")

