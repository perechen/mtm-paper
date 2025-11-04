################
### sampler ####
################

sample_lines <- function(x,n_lines=1000,n_samples=1,label=NA) {
  
  ## groups 
  if(!is.na(label)) {
    x <- x %>% mutate(label=str_remove(label, "_..?$"))
  }
  
  
  ### this to stratify further sampling by authors (i.e. take uniform distribution, but only for super rich) 
  ### those who wrote many lines in one meter
  major_poets <- x %>% count(author, label) %>% filter(n>5000) %>% mutate(gr_id=paste(author, label,sep="_"))
  
  ### two pools
  p1 <- x %>% mutate(gr_id=paste(author, label,sep="_")) %>% filter(gr_id %in% major_poets$gr_id) %>% select(-gr_id)
  p2 <- x %>% anti_join(p1 %>% select(line_id),by="line_id")
  
  ### if data has major poets
  if(nrow(major_poets) != 0) {
    p1 <- p1 %>% group_by(label, author) %>% sample_n(5000) %>% ungroup()
  }
  
  pool <- bind_rows(p1,p2)
  ### end of stratification
  
  
  ## quick way to make independent samples without replacement: take n_lines*n_samples worth of lines, then shuffle, and divide to n_sample buckets
  sample_out <- pool %>% 
    group_by(label) %>% 
    sample_n(n_lines*n_samples) %>% # take all that is required
    slice_sample(prop=1) %>% # shuffle within group
    mutate(sample_no=ceiling(row_number()/n_lines)) %>% # annotate buckets 
    ungroup() 
  
  return(sample_out)
}

sample_range <- function(x,n_lines=1000, n_samples_max=10, label=NA) {
  
  ## groups 
  if(!is.na(label)) {
    x <- x %>% mutate(label=str_remove(label, "_..?$"))
  }
  
  
  ### this to stratify further sampling by authors (i.e. take uniform distribution, but only for super rich) 
  ### those who wrote many lines in one meter
  major_poets <- x %>% count(author, label) %>% filter(n>5000) %>% mutate(gr_id=paste(author, label,sep="_"))
  
  ### two pools
  p1 <- x %>% mutate(gr_id=paste(author, label,sep="_")) %>% filter(gr_id %in% major_poets$gr_id) %>% select(-gr_id)
  p2 <- x %>% anti_join(p1 %>% select(line_id),by="line_id")
  
  ### if data has major poets
  if(nrow(major_poets) != 0) {
    p1 <- p1 %>% group_by(label, author) %>% sample_n(5000) %>% ungroup()
  }
  
  pool <- bind_rows(p1,p2)
  ### end of stratification
  
  
  forms <- x %>% count(label,sort=T) %>% mutate(s_pool = floor(n/n_lines),
                                                 is_minor= ifelse(s_pool < n_samples_max, T, F))
  ## quick way to make independent samples without replacement: take n_lines*n_samples worth of lines, then shuffle, and divide to n_sample buckets
  sample_out <- pool %>% 
    filter(!label %in% forms$label[forms$is_minor]) %>% 
    group_by(label) %>% 
    sample_n(n_lines*n_samples_max) %>% # take all that is required
    slice_sample(prop=1) %>% # shuffle within group
    mutate(sample_no=ceiling(row_number()/n_lines)) %>% # annotate buckets 
    ungroup() 
  
  s1 <- pool %>% 
    filter(label %in% forms$label[forms$is_minor]) %>%
    group_by(label) %>% left_join(forms %>% select(label, s_pool)) %>% group_split()
  
  sample_minor<-lapply(s1, function(x) {
    x %>% sample_n(n_lines*unique(s_pool)) %>% # take all that is required
      slice_sample(prop=1) %>% # shuffle within group
      mutate(sample_no=ceiling(row_number()/n_lines))
  })
  
  sample_out <- bind_rows(sample_minor, sample_out)
  
  
  return(sample_out)
}

##################
### vectorizer ###
##################

### ftr list
# "token"
# "pos"
# "syl"
# "pos_syl"

# custom n-gram tokenizer

generate_sliding_ngrams <- function(x, n) {
  words <- unlist(stri_extract_all_regex(x, "\\b[\\p{L}\\d]+\\b"))
  if (n > length(words)) {
    return(character(0))  # Return empty if n is larger than number of words
  }
  ngrams <- sapply(1:(length(words) - n + 1), function(i) {
    paste(words[i:(i + n - 1)], collapse = " ")
  })
  return(ngrams)
}

ngrammy <- function(x,n=2) {
  #  nmatch <- "\\b\\w+\\b" # basic char match
  nmatch <- "\\b[\\p{L}\\d]+\\b" # alternative
  pattern <- paste0("(?=(", paste(rep(nmatch,n),collapse=" "), "))") # allow multiple n
  
  matches<-gregexpr(pattern, x, perl=TRUE)
  # a little post-processing needed to get the capture groups with regmatches
  attr(matches[[1]], 'match.length') <- as.vector(attr(matches[[1]], 'capture.length')[,1])
  r <- regmatches(x, matches)
  return(r)
}

vectorizer <- function(x,mff=50,ftr="token",ngram=1,scale=T) {
  
  ## unnest tokens
  if(ngram == 1) {
    
    long <- x %>%
      group_by(label, sample_no) %>% 
      unnest_tokens(input=!! sym(ftr),output=token,to_lower = F)
    
    ## if bigrams, cut to bigrams
    
  } else {
    
    ngr <- x %>%
      select(label, sample_no, !! sym(ftr)) %>% 
      rename(token = !! sym(ftr)) %>% 
      rowwise() %>%
      mutate(token = list(generate_sliding_ngrams(token, n=ngram)))
    
    long <- ngr %>%
      unnest(token) %>% 
      ungroup()
    
  }
  
  ## count total 
  mf <- long %>% ungroup() %>%  count(token,sort=T) %>% pull(token)
  
  ## wide table
  wide <- long %>% 
    ungroup() %>% 
    count(label, sample_no, token) %>%
    mutate(token=factor(token, levels=mf)) %>%
    pivot_wider(names_from = token,values_from = n,values_fill = 0,names_sort=T)
  
  dtm <- wide[,-c(1,2)] %>% as.matrix()
  rownames(dtm) <- paste(wide$label, wide$sample_no,sep="_")
  
  
  ## handle if we want to grab all features
  if(is.na(mff)) {
    mff <- ncol(dtm)
  }
  
  dtm_s <- dtm[,1:mff]/rowSums(dtm[,1:mff])
  dtm_s[which(is.na(dtm_s))] <- 0
  
  if(scale) {
    dtm_s <- scale(dtm_s)
  }
  
  return(dtm_s)
}

## LOO for random Forest
loo_cv <- function(x,df) {
  
  rf <- randomForest(meter1~., data=df[-x,],nodesize=1,ntree=500)
  r <- predict(rf,train[x,])
  return(as.character(r))
  
}

rf_kfold <- function(df,d,fold=0.2,n_folds=25,iter=i) {
  # rows of classes
  class_rows <- lapply(unique(rownames(d)), function(x) {which(x == rownames(d))})
  names(class_rows) <- unique(rownames(d))
  
  cm_list <-  vector("list", length=n_folds)
  # samples available
  s <- ceiling(table(rownames(d))*fold)
  res_acc <-  vector("logical", length=n_folds)
  res_f1 <- vector("logical", length=n_folds)
  
  for(nf in 1:n_folds) {
    
  samples_test <- vector("list", length=length(class_rows))
  
  for(i in 1:length(class_rows)) {

    samples_test[[i]]<-class_rows[[i]] %>% sample(size=s[i])
  }
  
  samples_test <- unlist(samples_test)
  true <- factor(rownames(d)[samples_test])
  
  rf <- randomForest(meter1~., data=df[-samples_test,],nodesize=1,ntree=500)
  r <- predict(rf,train[samples_test,])
  
  cm <- confusionMatrix(r, true ,mode = "prec_recall")
  
  if(nf == 1) {
    cm_macro <- cm$table
  } else {
    cm_macro <- cm_macro + cm$table
  }
  

  
  }
  
  cm_macro %>% write.table(file=paste0("results/conf_matrices/", 
                                       langs[c], "_",
                                       f, "_",
                                       n, "_",
                                       ng, "_",
                                       a_type, "_",
                                       iter))
  
  cm0 <- confusionMatrix(cm_macro,mode = "prec_recall")
  res_acc <- cm0$overall[1] # accuracy
  if(is.null(nrow(cm0$byClass))) {
    res_f1 <- mean(cm0$byClass[7]) %>% round(3) 
  } else {
  res_f1 <- mean(cm0$byClass[,7]) %>% round(3)  # macro F1
  }
  return(list(res_acc,res_f1,cm_macro))

}

################
### drawings ###
################


plt <- c(
  "#003f5c",
  "#444e86",
  "#955196",
  "#dd5182",
  "#ff6e54",
  "#ffa600"
)


draw_tree_meters <- function(x,is_dtm=T,labels,groupings,hnode=NA, pal=2,col=1,skip=1,ord=c(1,2)) {
  dm <- as.matrix(x)
  if(is_dtm) {
    dm <- philentropy::JSD(as.matrix(x))
  } 
  
  
  
  
  ## tree object
  dm <- dm  %>% 
    `rownames<-`(labels) %>% # reset rownames
    as.dist() %>% # to dist object
    hclust(method="ward.D2") %>%
    ape::as.phylo()
  
  ## groups
  tree <- groupOTU(dm,groupings)
  
  plot=ggtree(tree,aes(color=group),layout="circular",size=1) 
  plot$data <- plot$data  %>%
    mutate(class = str_remove_all(label, "_.*?$"),
           label = str_remove_all(label, " .*?$"), # get class labels
           group=str_replace(group, "0", "A")) 
  #  left_join(trans, by="class")  %>% # translations
  root <- plot$data %>% filter(parent==node) %>% pull(node)
  print(root)
  
  col_seq <- c(5,2,1)
  ## to highlight the binary split
  if(is.na(hnode) %>% unique()) {
    high_nodes <- c(root+1,root+2)
    d <- data.frame(node=high_nodes, foot=c("ternary", "binary"))
    pal <- paletteer_d("beyonce::X2")[col_seq[ord]]
  } else {
    # custom node clust
    high_nodes <- hnode
    d <- data.frame(node=high_nodes, foot=LETTERS[1:length(hnode)])
    pal <- paletteer_d("beyonce::X2")[col]
  }
  
  
  p <- plot +
    geom_highlight(data=d,aes(node=node, fill=foot),type="auto",alpha=0.3) +
    geom_tiplab(aes(label=label),hjust=-.1,size=3) + 
    guides(color="none",fill="none") +
    scale_fill_manual(values=pal) +
    scale_color_manual(values=paletteer_d("beyonce::X2")[-skip]) 
  
  return(p)
  
  
} 
draw_scatter <- function(df, xpos, ypos, plt,filter=T) {
  
  tibble(x=df[,xpos],
         y=df[,ypos],
         label0=rownames(df)) %>% 
    mutate(label=str_remove(label0,"_.*")) %>% 
    {if (filter) filter(., label %in% c("iamb", "trochee")) else .} %>% 
    ggplot(aes(x,y,color=label)) + 
    geom_point(size=2,shape = 1,stroke = 1) + 
    theme_minimal() + 
    scale_color_manual(values=plt) + 
    theme(axis.title = element_text(size=10), legend.title=element_blank(),plot.title = element_text(size=14),
          plot.background = element_rect(fill="white",color=NA)) + labs(x=colnames(df)[xpos], y=colnames(df)[ypos])
  
}


## transitional probabilities

build_transitions <- function(x,m="iamb_4",l="ru"){
  
  df1 <- x %>% filter(lang==l)  %>% filter(label==m)
  
  df_tp <- df1 %>% 
    mutate(pos_syl = paste0("SOL ",pos_syl, " EOL")) %>% 
    select(poem_id,line_id,pos_syl) %>% 
    unnest_tokens(output = "ng", input = "pos_syl", token = "ngrams",n=2,drop = F,to_lower = F)
  
  df_tp_n3 <- df1 %>% 
    mutate(pos_syl = paste0("SOL ",pos_syl, " EOL")) %>% 
    select(poem_id,line_id,pos_syl) %>% 
    #  filter(poem_id %in% c(1:500)) %>% 
    unnest_tokens(output = "ng", input = "pos_syl", token = "ngrams",n=3,drop = F,to_lower = F)
  
  df_tp_n4 <- df1 %>% 
    mutate(pos_syl = paste0("SOL ",pos_syl, " EOL")) %>% 
    select(poem_id,line_id,pos_syl) %>% 
    # filter(poem_id %in% c(1:500)) %>% 
    unnest_tokens(output = "ng", input = "pos_syl", token = "ngrams",n=4,drop = F,to_lower = F)
  
  df_tp_n5 <- df1 %>% 
    mutate(pos_syl = paste0("SOL ",pos_syl, " EOL")) %>% 
    select(poem_id,line_id,pos_syl) %>% 
    # filter(poem_id %in% c(1:500)) %>% 
    unnest_tokens(output = "ng", input = "pos_syl", token = "ngrams",n=5,drop = F,to_lower = F)
  
  
  ## transitional probabilities for n=2
  tp <- df_tp %>%
    mutate(ng2 = ng) %>%
    separate(ng2,into = c("source", "target"),sep=" ") %>%
    count(source,target,sort = T) %>%
    mutate(p=n/sum(n)) %>%
    select(-n) %>% 
    pivot_wider(names_from = target,values_from = p,values_fill = 0)
  
  ## transitional probabilities for n=3
  tp3 <- df_tp_n3 %>%
    mutate(ng2 = ng) %>%
    separate(ng2,into = c("source", "source2", "target"),sep=" ") %>%
    unite("source",c(source, source2),sep = " ") %>%
    count(source,target,sort = T) %>%
    mutate(p=n/sum(n)) %>%
    select(-n) %>% 
    pivot_wider(names_from = target,values_from = p,values_fill = 0)
  
  ## transitional probabilities for n=4
  tp4 <- df_tp_n4 %>%
    mutate(ng2 = ng) %>%
    separate(ng2,into = c("source", "source2","source3", "target"),sep=" ") %>%
    unite("source",c(source, source2, source3),sep = " ") %>%
    count(source,target,sort = T) %>%
    mutate(p=n/sum(n)) %>% select(-n) #%>% pivot_wider(names_from = target,values_from = p,values_fill = 0)
  
  v<-tp4$target %>% table()
  tp4 <- tp4 %>% filter(target %in% names(v)) %>% pivot_wider(names_from = target,values_from = p,values_fill = 0) 
  
  tp5 <- df_tp_n5 %>%
    mutate(ng2 = ng) %>%
    separate(ng2,into = c("source", "source2","source3", "source4", "target"),sep=" ") %>%
    unite("source",c(source, source2, source3,source4),sep = " ") %>%
    count(source,target,sort = T) %>%
    filter(!str_detect(source, "^NA")) %>% 
    mutate(p=n/sum(n)) %>% select(-n) %>% 
    pivot_wider(names_from = target,values_from = p,values_fill = 0)
  
  
  cols <- colnames(tp)[-1]
  rows <- tp$source
  
  cols3 <- colnames(tp3)[-1]
  rows3 <- tp3$source
  
  cols4 <- colnames(tp4)[-1]
  rows4 <- tp4$source
  
  
  cols5 <- colnames(tp5)[-1]
  rows5 <- tp5$source
  
  tp_matrix <- as.matrix(tp[,-1])
  rownames(tp_matrix) <- rows
  
  tp3_matrix <- as.matrix(tp3[,-1])
  rownames(tp3_matrix) <- rows3
  
  tp4_matrix <- as.matrix(tp4[,-1])
  rownames(tp4_matrix) <- rows4
  
  tp5_matrix <- as.matrix(tp5[,-1])
  rownames(tp5_matrix) <- rows5
  
  tp_list <- list(tp_matrix, tp3_matrix, tp4_matrix,tp5_matrix)
  return(tp_list)
}





max_ll <- function(tp_list,maxll=F) {
  x_line <- c("SOL")
  start <- "SOL"
  
  ## first sample
  if(length(x_line) == 1) {
    #  while(!"EOL" %in% x_line) {
    
    if(!maxll) {
      next_tok <- names(sample(tp_list[[1]][start,],size = 1,prob=tp_list[[1]][start,]))
    } else {
      next_tok <- names(which.max(tp_list[[1]][start,]))
    }
    
    start <- paste(x_line, next_tok)
    
    x_line = c(x_line,next_tok)
    
    
    #}
  }
  
  #while(!"EOL" %in% x_line) {
  
  if(!maxll) {
    next_tok <- names(sample(tp_list[[2]][start,],size = 1,prob=tp_list[[2]][start,])) } else {
      next_tok <- names(which.max(tp_list[[2]][start,]))
    }
  
  x_line = c(x_line,next_tok)
  
  start <- paste(x_line,collapse = " ")
  
  
  while(!"EOL" %in% x_line) {
    
    if(!maxll) {
      next_tok <- names(sample(tp_list[[3]][start,],size = 1,prob=tp_list[[3]][start,])) } else {
        next_tok <- names(which.max(tp_list[[3]][start,]))
      }
    
    x_line = c(x_line,next_tok)
    l <- length(x_line)
    start <- paste(x_line[c(l-2,l-1,l)],collapse = " ")
    
    
  }
  return(x_line)
  
  
}


