# main
library(tidyverse)
library(tidytext)
library(stringi)
library(caret)

# plots
library(patchwork)
library(paletteer)
library(scales)

# scripts
source("00_scripts.R")

########################
### data preparation ###
########################


min_lines = 5000 ## filter meters by size # 1000k x 5 samples

fs <- c("data/dump/cs/", "data/dump/de", "data/dump/ru/")
#file_names <- c("data/prepared_foot/cs_lines.tsv", "data/prepared_foot/de_lines.tsv", "data/prepared_foot/ru_lines.tsv")
file_names <- c("data/prepared/cs_lines.tsv", "data/prepared/de_lines.tsv", "data/prepared/ru_lines.tsv")

## iterate over folders
for(i in 1:3) {
### file names
f<-list.files(fs[i],full.names = T)

### collect
cs <- lapply(f, read_csv, 
             col_names = c("poem_id", "line_id","token", "pattern", "pos", "author", "born","died"),
             col_types = list("i", "i", "c", "c", "c", "c","i", "i"))

### table with metrical labels
labels <- tibble(meter=as.character(1:length(cs)),
                 label=str_replace(f,".*/(.*?)\\.csv", "\\1"),
                 label_foot=str_remove(label, "_..?$"))

### combine tables
cs_df <- cs %>% bind_rows(.id = "meter") %>% left_join(labels)

## filter by birth year
if(i == 1 | i == 2) {
  cs_df <- cs_df %>% filter(born >= 1800, born <= 1900)
}

## filter meters
selection <- cs_df %>%
  count(label,line_id) %>%
  count(label,sort=T) %>% 
  filter(n > min_lines)

## apply selection to the larger table
cs_df <- cs_df %>% filter(label %in% selection$label)

### German POS simplification ### 

if(i == 2) {
de_pos <- read_csv("data/german_simplification.csv")
cs_df <- cs_df %>% left_join(de_pos,by="pos") %>% select(-pos) %>% rename(pos=pos_target)
}

## cast lines: one line = one row
lines_df <- cs_df %>%
  mutate(pos_syl=paste0(pos, nchar(pattern))) %>%
  group_by(meter, label, label_foot, poem_id, line_id,author,born,died) %>%
  summarize(token=paste(token, collapse=" "),
            pattern=paste(pattern, collapse=" "),
            pos=paste(pos,collapse=" "),
            pos_syl=paste(pos_syl, collapse=" "))

write_tsv(lines_df,file = file_names[i])
}

########################
### Corpus summaries ###
########################

cs_df <- read_tsv("data/prepared/cs_lines.tsv")  %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))
# CS lines
nrow(cs_df)
# CS number of foot types
cs_df$label_foot %>% table() %>% sort(decreasing = T)
cs_df$label_foot %>% table() %>% length()
# CS number of meters
cs_df$label %>% table() %>% sort(decreasing = T)
cs_df$label %>% table() %>% length()


de_df <- read_tsv("data/prepared/de_lines.tsv")  %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))
# DE lines
nrow(de_df)
# DE number of foot types
de_df$label_foot %>% table() %>% sort(decreasing = T)
de_df$label_foot %>% table() %>% length()
# DE number of meters
de_df$label %>% table() %>% sort(decreasing = T)
de_df$label %>% table() %>% length()


ru_df <- read_tsv("data/prepared/ru_lines.tsv")  %>% 
  mutate(syl= str_remove_all(pos_syl, "[A-Z]"))
# RU lines
nrow(ru_df)
# RU number of foot types
ru_df$label_foot %>% table() %>% sort(decreasing = T)
ru_df$label_foot %>% table() %>% length()
# RU number of meters
ru_df$label %>% table() %>% sort(decreasing = T)
ru_df$label %>% table() %>% length()


####################################
#### FIGURE 1. Czech & syllables ###
####################################


source("00_scripts.R")

## conjunction 'a' at the beginning of a line in iambic pentameter
conj_a <- cs_df %>%
  filter(label=="iamb_5") %>% 
  mutate(token=str_split(token, " ")) %>% 
  unnest(token) %>% 
  group_by(line_id) %>% 
  mutate(n_word=row_number()) %>% 
  filter(token=="a")

conj_a %>% ggplot(aes(n_word)) + geom_bar() + theme_minimal() + labs(title="Czech 'a' distribution in a I5 line")

## in percents
conj_a %>%
  ungroup() %>%
  count(n_word) %>%
  mutate(n=n/sum(n))

### sample & vectorize a little bit of data
set.seed(1989)
s <- sample_lines(cs_df,n_lines = 100,n_samples = 40,label = "foot")
d <- vectorizer(s,mff = NA,ngram = 1,ftr = "syl",scale=F)

### 4 meters by word length
p2 <- draw_scatter(d,xpos = 1,ypos = 2,plt=plt[c(3,5,4,6)],filter = F) + labs(x="Monosyllables",y="Disyllables",title = "b.")

### sample data for iamb vs. trochee
set.seed(119)
s <- sample_lines(cs_df,n_lines = 100,n_samples = 60,label = "foot")
d <- vectorizer(s,mff = NA,ngram = 1,ftr = "token",scale=F)

### dotplot!
p1<-tibble(x=d[,1],
           label0=rownames(d)) %>% 
  mutate(label=str_remove(label0,"_.*")) %>% 
  filter(label %in% c("iamb", "trochee")) %>% 
  ggplot(aes(x, fill=label)) + geom_dotplot(binwidth=0.0025,stackgroups = T, binpositions = "all") + 
  scale_fill_manual(values=plt[c(4,6)]) + theme_minimal() + #+ ylim(0,0.25)  + 
  theme(axis.title = element_text(size=10), plot.title = element_text(size=14), axis.text.y=element_blank(), plot.background = element_rect(fill="white",color=NA),legend.title=element_blank()) + 
  labs(x="Frequency of 'a' ('and') ", y=NULL, title="a.")  + guides(fill="none")


p1+p2
ggsave("plots/paper/Figure_1.png",width = 10,height = 5,dpi = 300)


#################################################
### Appendix. Figure 1. POS / Stress patterns ###
#################################################


## split lines to tokens
df_unnested <- de_df %>% 
  mutate(token=str_split(token,"\\s",),
         pattern=str_split(pattern,"\\s"),
         pos=str_split(pos,"\\s")) %>% 
  select(label,line_id, token, pattern,pos) %>% 
  unnest(c(token, pattern,pos)) 

## combine with syllable length
unnested_2 <- df_unnested %>%
  mutate(nsyl=nchar(pattern)) %>%
  group_by(line_id) %>%
  mutate(token_id=row_number(),pattern=str_split(pattern, "")) %>%
  unnest(c(pattern)) %>%
  group_by(line_id, token_id) %>% 
  mutate(syl_id=row_number())

## for relabeling 
de_labels <- tibble(pos=c("V", "T", "R", "P", "N", "J", "I","D","C", "ART","A"),
                    lab=c("Verb", "Particle", "Preposition", "Pronoun", "Noun", "Conjuction", "Interjection", "Adverb","Numeral","Article","Adjective")) %>% 
  mutate(lab=factor(lab,levels=rev(sort(unique(lab)))))

pge <- unnested_2 %>%
  ungroup() %>% 
  count(pattern, nsyl, pos,syl_id) %>%
  filter(nsyl > 1, nsyl < 7, pos != "##", pos != "I" , pos != "PUNCT", pos != "R",pos != "T", pos!="C", pos != "J", pos != "ART", pos != "TRUNC", pos != "FM", pattern=="1", pos !="XY") %>% 
  arrange(pos,nsyl,syl_id) %>%
  group_by(nsyl,pos) %>%
  mutate(n=n/sum(n)) %>% 
  rename(stressed=n) %>%  
  left_join(de_labels,by="pos") %>% 
  ggplot(aes(as.character(syl_id),lab, fill=stressed)) + 
  geom_tile() + 
  facet_wrap(~nsyl,scales = "free_x",nrow = 1) + 
  theme_bw(base_size = 14) + 
  theme(strip.background = element_rect(fill="white"),panel.grid = element_blank()) +
  scale_fill_viridis_c(option = "E") +
  #guides(fill="none") +
  labs(x=NULL,y=NULL,title="a. German") 



### POS RU

df_unnested <- ru_df %>% 
  mutate(token=str_split(token,"\\s",),
         pattern=str_split(pattern,"\\s"),
         pos=str_split(pos,"\\s")) %>% 
  select(label,line_id, token, pattern,pos) %>% 
  unnest(c(token, pattern,pos))  %>% 
  filter(pattern != "NA")

unnested_ru <- df_unnested %>%
  mutate(nsyl=nchar(pattern)) %>%
  group_by(line_id) %>%
  mutate(token_id=row_number(),pattern=str_split(pattern, "")) %>%
  unnest(c(pattern)) %>%
  group_by(line_id, token_id) %>% 
  mutate(syl_id=row_number())

ru_labels <- tibble(pos=c("S", "A", "PRO", "V", "CONJ", "ADV", "PR","INTJ","SPRO", "PART","ADVPRO", "ANUM", "NUM"),
                    lab=c("Noun", "Adjective", "Pronoun", "Verb", "Conjuction", "Adverb", "Preposition", "Interjection","S-Pronoun","Particle","Adv-pronoun","A-Numeral", "Numeral")) %>% 
  mutate(lab=factor(lab,levels=rev(sort(unique(lab)))))

pru <- unnested_ru %>%
  mutate(pos=ifelse(str_detect(pos, "PRO"), "PRO", pos)) %>% 
  ungroup() %>% 
  count(pattern, nsyl, pos,syl_id) %>%
  filter(nsyl > 1, nsyl < 7, pos != "##", pos != "COM", pattern!="0", !pos %in% c("PR", "CONJ", "PART", "NUM", "INTJ","ANUM")) %>% 
  group_by(nsyl,pos) %>%
  mutate(n=n/sum(n)) %>% 
  rename(stressed=n) %>%  
  left_join(ru_labels,by="pos") %>% 
  ggplot(aes(as.character(syl_id)
             ,lab, fill=stressed)) + 
  geom_tile() + 
  facet_wrap(~nsyl,scales = "free_x",nrow = 1) + 
  theme_bw(base_size = 14) + 
  #guides(fill="none") + 
  theme(strip.background = element_rect(fill="white")) +
  scale_fill_viridis_c(option="F") +
  labs(x="n-th syllable",y=NULL,title="b. Russian") 


pge / pru
ggsave("plots/paper/Appendix_Figure_1.png",width = 9,height = 5)


##########################################
### Table 2 & Morpho-syntactic cliches ###
##########################################

df <- bind_rows(cs_df %>% mutate(lang="cs"),
                ru_df %>% mutate(lang="ru"),
                de_df %>% mutate(lang="de"))

## 10 most frequent line-types per meter per language in most common forms
counts <- df %>%
  filter(label %in% c("trochee_4", "iamb_4")) %>% 
  count(label,pos_syl,lang, sort=T) %>% 
  mutate(hook=paste0(pos_syl,lang,label)) %>% 
  group_by(label,lang) %>%
  mutate(p=round(n/sum(n)*100,3)) %>%
  top_n(5) %>% 
  arrange(lang, label, -n)

set.seed(345)
ex <- df %>% mutate(hook=paste0(pos_syl,lang,label)) %>% 
  filter(hook %in% counts$hook) %>% 
  group_by(hook) %>% 
  sample_n(1)

counts <- counts %>% left_join(ex %>% select(hook,token)) %>% 
  select(-hook)
## table 2
write_csv(counts,file = "results/formulas_examples.csv")

## Trochee-4 and Iamb-4 relationships in RU (shared line types and line overlap)

iou_meters <- function(x,l="ru") {
counts_syl <- x %>%
  filter(lang==l,
         label %in% c("trochee_4", "iamb_4")) %>% 
  count(label,pos_syl,lang, sort=T) %>% 
  mutate(nsyl=str_extract_all(pos_syl, "[0-9]")) %>% 
  rowwise() %>% 
  mutate(nsyl=unlist(nsyl) %>% as.numeric() %>% sum())

i4 <- counts_syl %>% filter(label == "iamb_4",nsyl == 8)
t4 <- counts_syl %>% filter(label == "trochee_4",nsyl == 8) 

intersection <- intersect(i4$pos_syl,t4$pos_syl)
union <- union(i4$pos_syl,t4$pos_syl)


int_t4 <- counts_syl %>% filter(pos_syl %in% intersection,label=="trochee_4")
int_i4 <- counts_syl %>% filter(pos_syl %in% intersection,label=="iamb_4")

n_int <- sum(int_t4$n) + sum(int_i4$n)
n_total <- sum(i4$n) + sum(t4$n)

#i_prop <- sum(int_i4$n) / sum(i4$n)
#t_prop <- sum(int_t4$n) / sum(t4$n)


# what amount of lines in general? 
# only 40% of lines of the same length (8 syllables) come from templates shared across I4 and T4
#n_int/n_total 
cat(l, "##### I4 vs T4:", "\n", "  Percent of shared forms:", length(intersection)/length(union)*100, "\n", "  Percent of shared lines:", (n_int/n_total)*100,"\n")
}

iou_meters(df,l="ru") # 7.5% // 40%
iou_meters(df,l="cs") # 1.3% // 12%
iou_meters(df,l="de") # 1.4% // 5%

########################################################################
### Appendix. Figure 2. Maximum likelihood Markov chain / perplexity ###
########################################################################
source("00_scripts.R")

## given a matrix of transitional probabilities, calculate perplexity of a test sequences
perplexity_ngram_matrix <- function(trans_mat, test_seq, n = 3, unk_prob = 1e-12) {
  # trans_mat: numeric matrix where
  #   - rownames = context strings (tokens joined by " ")
  #   - colnames = next words
  # Each cell = P(next_word | context)
  #
  # test_seq: vector of tokens
  # unk_prob: probability used for unseen transitions
  
  if (is.null(rownames(trans_mat)) || is.null(colnames(trans_mat))) {
    stop("Both rownames (contexts) and colnames (words) must be set in trans_mat.")
  }
  
  L <- length(test_seq)
  if (L < n) stop("test_seq shorter than model order n.")
  
  loglik <- 0
  preds <- 0
  
  for (i in n:L) {
    context <- paste(test_seq[(i - (n - 1)):(i - 1)], collapse = " ")
    word <- test_seq[i]
    
    if (context %in% rownames(trans_mat) && word %in% colnames(trans_mat)) {
      p <- trans_mat[context, word]
    } else {
      p <- unk_prob
    }
    
    if (is.na(p) || p <= 0) p <- unk_prob
    
    loglik <- loglik + log(p)
    preds <- preds + 1
  }
  
  perplexity <- exp(-loglik / preds)
  
  return(list(perplexity = perplexity,
              avg_neg_loglik = -loglik / preds,
              tokens_predicted = preds))
}

## bootstrap perplexity measures
bootstrap_ppr <- function(tp,line,n_times=100) {
  res_v <- vector(length=n_times)
  
  for(i in 1:n_times) {
    
    string <- sample(line, 1) %>% str_split(" ") %>% unlist()
    ppr <- perplexity_ngram_matrix(tp, test_seq =string,n=3)
    res_v[i] <- ppr$perplexity
    
    
  }
  return(res_v)
}

## full pipeline: train model -> calculate bootstrapped perplexity on unseen data
ppr_to_trochee <- function(d,d2,la,set_seed=1989,n_tries=100,sample_size=100) {
  
  set.seed(set_seed)
  train_ids <- sample(1:nrow(d), size = floor(nrow(d)*0.8))
  df1 <- d[train_ids,]
  df2 <- d[-train_ids,]
  
  tp_i4 <- build_transitions(df1,m = "iamb_4",l=la)
  
  
  lines <- df2 %>% pull(pos_syl)
  lines_iamb <- paste("SOL", lines, "EOL")
  
  linest <- d2 %>% filter(label=="trochee_4",lang==la) %>% pull(pos_syl)
  lines_trochee <- paste("SOL", linest, "EOL")
  
  
  i_v <- vector(length=n_tries)
  t_v <- vector(length=n_tries)
  
  for(i in 1:n_tries) {
    
    ppr_i <- bootstrap_ppr(tp_i4[[2]],lines_iamb,n_times = sample_size) %>% round(3)
    i_v[i] <- median(ppr_i)
    ppr_t <- bootstrap_ppr(tp_i4[[2]],lines_trochee,n_times = sample_size) %>% round(3)
    t_v[i] <- median(ppr_t)
  }
  return(list(i_v,t_v))
}

## the function here calculates transitional probabilities for a line from data
## it is based on predicting next token from the preceding n-gram,up to n=3
## it starts with Start of Line symbol (n=1), picks next token, then picks next token based on probabilities of preceding 2-gram (SOL+token1) 
de_train <- df %>% filter(label=="iamb_4",lang=="de")
cs_train <- df %>% filter(label=="iamb_4",lang=="cs")
ru_train <- df %>% filter(label=="iamb_4",lang=="ru")

ppr_de <- ppr_to_trochee(de_train,d2=df,la = "de",n_tries = 1000,sample_size = 500,set_seed = 1989)
ppr_cs <- ppr_to_trochee(cs_train,d2=df,la = "cs",n_tries = 1000,sample_size = 500,set_seed = 1989)
ppr_ru <- ppr_to_trochee(ru_train,d2=df,la = "ru",n_tries = 1000,sample_size = 500,set_seed = 1989)


tibble(ppr=unlist(ppr_de),
       meter=c(rep("iamb4",1000),
               rep("trochee4",1000)),
       lang="de") %>%
  bind_rows(tibble(ppr=unlist(ppr_cs),
                   meter=c(rep("iamb4",1000),
                           rep("trochee4",1000)),
                   lang="cs"),
            tibble(ppr=unlist(ppr_ru),
                   meter=c(rep("iamb4",1000),
                           rep("trochee4",1000)),
                   lang="ru") ) %>% 
#  filter(ppr<500) %>% 
  ggplot(aes(meter,ppr,color=meter,fill=meter)) +
  geom_boxplot(alpha=0.2) + 
  theme_minimal() + 
  theme(strip.text = element_text(size=14)) +
  scale_fill_manual(values=plt[c(6,4)]) +
  scale_color_manual(values=plt[c(6,4)]) + 
  labs(y="Perplexity",x=NULL,
       #title="Boostrapped median perplexity (Czech)",
       #subtitle="Iambic POS SYL language model, out-of-sample calculations, 10000 x 100 lines"
  ) +
  coord_flip() + 
  guides(color="none", fill="none") +
  facet_wrap(~lang,scales = "free_x",nrow = 3)


ggsave("plots/paper/perplexity.png",width = 8,height = 6)


## ML Markov chain
tp_i4 <- build_transitions(df,m = "iamb_4",l="ru")
tp_t4 <- build_transitions(df,m = "trochee_4",l="ru")

## some random generated lines of T4 vs I4
max_ll(tp_i4,maxll = F)
max_ll(tp_t4,maxll = F)

## maximum likelihood line for iamb-4
max_ll(tp_i4,maxll = T)
## maximum likelihood line for trochee-4
max_ll(tp_t4,maxll = T)



##########################################
#### Figure 2. Classification results ####
##########################################


resdf<-read_csv("results/res_foot_kfold.csv",col_names = c("lang","feature","sample_size","ngram","acc", "f1")) %>% 
  mutate(feature=ifelse(feature=="token","word",feature))


## classification by foot
xlines <- tibble(lang=c("cs", "de", "ru"),
                 baseline=c(0.25, 0.5, 0.20))

lang_labels <- c(
  "cs" = "Czech",
  "de" = "German",
  "ru" = "Russian"
)

p1 <- resdf %>% mutate(ngram=as.character(ngram)) %>% 
  group_by(lang,feature,sample_size,ngram) %>%
  summarize(lo=quantile(acc,0.025),
            hi=quantile(acc,0.975),
            acc=mean(acc)) %>% 
  ggplot(aes(sample_size, acc)) + 
  geom_line(aes(linetype=ngram,color=feature),linewidth=0.5) + 
  #geom_ribbon(aes(ymin=lo,ymax=hi,group=interaction(feature,ngram)),fill="grey", alpha=0.3) +
  facet_wrap(~lang,labeller = as_labeller(lang_labels)) + 
  labs(x=NULL,y="Accuracy",title = "a. Classifying by foot type (2-5 classes)") + 
  scale_color_paletteer_d("basetheme::clean") + 
  theme_minimal() + 
  geom_hline(data=xlines,aes(yintercept=baseline),linetype=3) + 
  theme(strip.text = element_text(size=14),
        plot.background = element_rect(fill="white",color=NA),
        plot.title = element_text(size=10)) +
  scale_y_continuous(breaks = seq(0,1,by=0.1)) + guides(linetype="none", color="none") #+ 
scale_x_log10()

p1
resdf<-read_csv("results/res_form_kfold.csv",col_names = c("lang","feature","sample_size","ngram","acc","f1")) %>% 
  mutate(feature=ifelse(feature=="token","word",feature))



xlines <- tibble(lang=c("cs", "de", "ru"),
                 baseline=c(0.0625, 0.2, 0.06))


## classification by meter
p2 <- resdf %>% mutate(ngram=as.character(ngram)) %>% 
  group_by(lang,feature,sample_size,ngram) %>%
  summarize(lo=quantile(acc,0.025),
            hi=quantile(acc,0.975),
            acc=mean(acc)) %>% 
  ggplot(aes(sample_size, acc)) + 
  geom_line(aes(linetype=ngram,color=feature),linewidth=0.5) + 
  #  geom_ribbon(aes(ymin=lo,ymax=hi,group=interaction(feature,ngram)),fill="grey", alpha=0.3) +
  facet_wrap(~lang,labeller = as_labeller(c("Czech", "German", "Russian"))) + 
  labs(x="Sample size (lines)",y="Accuracy",title = "b. Classifying by metrical form (5-17 classes)") + 
  scale_color_paletteer_d("basetheme::clean") + 
  theme_minimal() + 
  geom_hline(data=xlines,aes(yintercept=baseline),linetype=3) + 
  theme(strip.text = element_blank(),
        plot.background = element_rect(fill="white",color=NA,),plot.title = element_text(size=10),legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0,1,by=0.1))

## combine (god bless patchwork)
p1/p2


ggsave("plots/paper/Figure_2.png",height=6,width=8,dpi = 300)


################################################
#### Appendix Figure 3 & confusion matrices ####
################################################

sum_matrix_df <- function(df,l="cs",f="token",s=100,ng=1,t="foot") {
  
  cm_group <- df %>% 
    filter(lang==l,
           feature==f,
           size==s,
           ngram==ng,
           type==t)
  
  cms <- lapply(cm_group$path, read.table)
  
  ## sum confusion matrices
  for(i in 1:length(cms)) {
    m0 <- cms[[i]]
    if(i == 1) {
      m <- m0
    }
    if(i > 1) {
      m <- m + m0
    }
  }
  
  cm_df <- as_tibble(m) %>%
    mutate(expected=rownames(m),
           lang=l,
           feature=f,
           size=s,
           ngram=ng,
           type=t) %>% 
    pivot_longer(cols = 1:nrow(m),names_to = "predicted") %>% 
    mutate(expected = factor(expected,levels = rev(rownames(m)))) 
  return(cm_df)
  
}

make_plots_cm <- function(lang="cs",l_lab="Czech",type="NA",text_size=1.5) {
  plot_list <- vector(mode="list",length = 8)
  cntr <- 0
  
  
  # for lang / for feat / for ngr -. save to list - make plot
  
  for(fi in 1:length(feat)) {
    
    for(ni in 1:length(ngr)) {
      cntr=cntr+1
      
      for(i in 1:length(s)) {
        
        l[[i]] <- sum_matrix_df(df,s = s[i],f = feat[fi],ng=ni,l=lang,t=type) 
      }
      
      
      plot_list[[cntr]] <- l %>% bind_rows() %>% 
        group_by(predicted,size) %>% 
        mutate(value= round(value / sum(value),2)) %>% 
        ungroup() %>% 
        mutate(is_bright = value > 0.5,
               label = case_when(
                 value == 0 ~ "0",
                 TRUE ~ {
                   s <- sprintf("%.2f", value)               
                   s <- sub("^(-?)0\\.", "\\1.", s)         
                   sub("\\.?0+$", "", s)                
                 }
               )) %>% 
        ggplot(aes(expected, predicted)) + 
        facet_wrap(~size, nrow = 1) +
        geom_tile(aes(fill = value)) + 
        coord_flip() + 
        geom_text(aes(label = label, color = is_bright), size = text_size) + 
        scale_fill_viridis_c(option = "A") + 
        scale_color_manual(values = c("white","black")) + 
        guides(fill = "none", color = "none") + 
        theme_light() + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
        labs(title = paste0(l_lab, ", ", feat[fi], ", ", ngr[ni], "gram"))
      
    }
  }
  return(plot_list)
}

f <- list.files("results/conf_matrices/",full.names = T)

df <- tibble(path=f) %>%
  mutate(path2 = path,
         path2 = str_replace(path2, "pos_syl", "possyl"),
         feat=str_remove(path2,"^.*?//")) %>% 
  separate(feat, c("lang", "feature","size","ngram","type","i"))



lang <- "cs" # "cs" "de" "ru"
l_lab <- "Czech"
type="NA"



feat <- c("syl","pos", "possyl", "token") 
ngr <- c(1,2)



s <- as.numeric(df$size) %>% unique()
l <- vector(mode="list",length=8)



## Czech plots

plots_cs <- make_plots_cm(lang="cs",l_lab = "Czech",type="NA")
plots_cs[[1]] / plots_cs[[2]] / plots_cs[[3]] / plots_cs[[4]] / plots_cs[[5]] / plots_cs[[6]] / plots_cs[[7]] / plots_cs[[8]]

ggsave("plots/CM_plots/CS_forms.png",width = 20,height = 32)

plots_cs <- make_plots_cm(lang="cs",l_lab = "Czech",type="foot")
plots_cs[[1]] / plots_cs[[2]] / plots_cs[[3]] / plots_cs[[4]] / plots_cs[[5]] / plots_cs[[6]] / plots_cs[[7]] / plots_cs[[8]]

ggsave("plots/CM_plots/CS_foot.png",width = 10,height = 20)

## Russian plots

plots_ru <- make_plots_cm(lang="ru",l_lab = "Russian",type="NA")
plots_ru[[1]] / plots_ru[[2]] / plots_ru[[3]] / plots_ru[[4]] / plots_ru[[5]] / plots_ru[[6]] / plots_ru[[7]] / plots_ru[[8]]

ggsave("plots/CM_plots/RU_forms.png",width = 20,height = 32)

plots_ru <- make_plots_cm(lang="ru",l_lab = "Russian",type="foot")
plots_ru[[1]] / plots_ru[[2]] / plots_ru[[3]] / plots_ru[[4]] / plots_ru[[5]] / plots_ru[[6]] / plots_ru[[7]] / plots_ru[[8]]

ggsave("plots/CM_plots/RU_foot.png",width = 10,height = 20)

## German plots

plots_de <- make_plots_cm(lang="de",l_lab = "German",type="NA")
plots_de[[1]] / plots_de[[2]] / plots_de[[3]] / plots_de[[4]] / plots_de[[5]] / plots_de[[6]] / plots_de[[7]] / plots_de[[8]]

ggsave("plots/CM_plots/DE_forms.png",width = 10,height = 20)

plots_de <- make_plots_cm(lang="de",l_lab = "German",type="foot",text_size = 3)
plots_de[[1]] / plots_de[[2]] / plots_de[[3]] / plots_de[[4]] / plots_de[[5]] / plots_de[[6]] / plots_de[[7]] / plots_de[[8]]

ggsave("plots/CM_plots/DE_foot.png",width = 10,height = 20)
