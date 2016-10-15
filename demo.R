
rm(list=ls())
library(grid)
library(lda)
library(pracma)
library(tm)

path = "training/class0"
path1 = "training/class1"
path2 = "training/class2"
path3 = "training/class3"
path4 = "training/class5"

filelist = list.files(path, pattern = ".*.txt")
filelist1 = list.files(path1, pattern = ".*.txt")
filelist2 = list.files(path2, pattern = ".*.txt")
filelist3 = list.files(path3, pattern = ".*.txt")
filelist4 = list.files(path4, pattern = ".*.txt")

to_rep = "said|stated|much|like|contents"   //remove some words

A=c()
for( y in filelist)
{
 c = paste(path,y,sep="/")

  fileName <- c
  conn <- file(fileName,open="r")
  linn <-readLines(conn)
  for (i in 1:length(linn))
  {
    if(!grepl("classmwheadline", linn[i]))
    {
     if(linn[i]!=" " && linn[i]!="" & linn[i]!=" " && linn[i]!='')
     {
      stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
      stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
      doc = stringr::str_replace_all(linn[i], stopwords_regex, '')
      
      doc = gsub(" *\\b[[:alpha:]]{1,2}\\b *"," ",doc)
      doc = gsub(to_rep," ", doc)
      doc = gsub("\\s+"," ",doc)
      doc = str_trim(doc, side = c("both"))
      
      if(doc!="")
      {
        A <- append(A,doc)
      } 
     }
   }  
  }
  close(conn)
}

length(A)


B=c()
for( y in filelist1)
{
  #print(y)
  d = paste(path1,y,sep="/")
  
  fileName <- d
  conn <- file(fileName,open="r")
  linn <-readLines(conn)
  for (i in 1:length(linn))
  {
    if(!grepl("classmwheadline", linn[i]))
    {
      if(linn[i]!=" " && linn[i]!="" & linn[i]!=" " && linn[i]!='')
      {
        stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
        stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
        doc = stringr::str_replace_all(linn[i], stopwords_regex, '')
        
        doc = gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ",doc)
        doc = gsub(to_rep," ", doc)
        doc = gsub("\\s+"," ",doc)
        doc = str_trim(doc, side = c("both"))
        
        if(doc!="")
        {
          B <- append(B,doc)
        } 
      }
    }  
  }
  close(conn)
}

length(B)


C=c()
for( y in filelist2)
{
  c = paste(path2,y,sep="/")
  
  fileName <- c
  conn <- file(fileName,open="r")
  linn <-readLines(conn)
  for (i in 1:length(linn))
  {
    if(!grepl("classmwheadline", linn[i]))
    {    
     if(linn[i]!=" " && linn[i]!="" & linn[i]!=" " && linn[i]!='')
     {
      stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
      stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
      doc = stringr::str_replace_all(linn[i], stopwords_regex, '')
      
      doc = gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ",doc)
      doc = gsub(to_rep," ", doc)
      doc = gsub("\\s+"," ",doc)
      doc = str_trim(doc, side = c("both"))
      
      if(doc!="")
      {
        C <- append(C,doc)
      } 
     }
    }  
  }
  close(conn)
}

length(C)

D=c()
for( y in filelist3)
{
  c = paste(path3,y,sep="/")
  
  fileName <- c
  conn <- file(fileName,open="r")
  linn <-readLines(conn)
  for (i in 1:length(linn))
  {
    if(!grepl("classmwheadline", linn[i]))
    {
      if(linn[i]!=" " && linn[i]!="" & linn[i]!=" " && linn[i]!='')
      {
        stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
        stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
        doc = stringr::str_replace_all(linn[i], stopwords_regex, '')
        
        doc = gsub(" *\\b[[:alpha:]]{1,2}\\b *"," ",doc)
        doc = gsub(to_rep," ", doc)
        doc = gsub("\\s+"," ",doc)
        doc = str_trim(doc, side = c("both"))
        
        if(doc!="")
        {
          D <- append(D,doc)
        } 
      }
    }  
  }
  close(conn)
}

length(D)


E=c()
for( y in filelist4)
{
  c = paste(path4,y,sep="/")
  
  fileName <- c
  conn <- file(fileName,open="r")
  linn <-readLines(conn)
  for (i in 1:length(linn))
  {
    if(!grepl("classmwheadline", linn[i]))
    {
      if(linn[i]!=" " && linn[i]!="" & linn[i]!=" " && linn[i]!='')
      {
        stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
        stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
        doc = stringr::str_replace_all(linn[i], stopwords_regex, '')
        
        doc = gsub(" *\\b[[:alpha:]]{1,2}\\b *"," ",doc)
        doc = gsub(to_rep," ", doc)
        doc = gsub("\\s+"," ",doc)
        doc = str_trim(doc, side = c("both"))
        
        if(doc!="")
        {
          E <- append(E,doc)
        } 
      }
    }  
  }
  close(conn)
}

length(E)


#-----------------------------------------------

# "Early Life" <- 0
# "Personal Life" <- 1
# "Career" <- 2
# "Controversies <- 3
# "Later" <- 4

#-----------------------------------------------

# For test

path3 = "test/test0"
filelist3 = list.files(path3, pattern = ".*.txt")

Test1=c()
for(y in filelist3)
{
  # print(y)
  c = paste(path3,y,sep="/")
  
  fileName <- c
  conn <- file(fileName,open="r")
  linn <-readLines(conn)
  for (i in 1:length(linn))
  {
    if(!grepl("classmwheadline", linn[i]))
    {    
      if(linn[i]!=" " && linn[i]!="" & linn[i]!=" " && linn[i]!='')
      {
        stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
        stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
        doc = stringr::str_replace_all(linn[i], stopwords_regex, '')
        
        doc = gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ",doc)
        doc = gsub(to_rep," ", doc)
        doc = gsub("\\s+"," ",doc)
        doc = str_trim(doc, side = c("both"))
        
        if(doc!="")
        {
          Test1 <- append(Test1,doc)
        } 
      }
    }  
  }
  close(conn)
}

lent = length(Test1)
print(lent)  


path4 = "test/test1"
filelist4 = list.files(path4, pattern = ".*.txt")
  
  Test2=c()
  for( y in filelist4)
  {
    # print(y)
    c = paste(path4,y,sep="/")
    
    fileName <- c
    conn <- file(fileName,open="r")
    linn <-readLines(conn)
    for (i in 1:length(linn))
    {
      if(!grepl("classmwheadline", linn[i]))
      {    
        if(linn[i]!=" " && linn[i]!="" & linn[i]!=" " && linn[i]!='')
        {
          stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
          stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
          doc = stringr::str_replace_all(linn[i], stopwords_regex, '')
          
          doc = gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ",doc)
          doc = gsub(to_rep," ", doc)
          doc = gsub("\\s+"," ",doc)
          doc = str_trim(doc, side = c("both"))
          
          if(doc!="")
          {
            Test2 <- append(Test2,doc)
          } 
        }
      }  
    }
    close(conn)
  }
  
  lent1 = length(Test2)
  print(lent1)
  
  
  path5 = "test/test2"
  filelist5 = list.files(path5, pattern = ".*.txt")
  
  Test3=c()
  for( y in filelist5)
  {
    # print(y)
    c = paste(path5,y,sep="/")
    
    fileName <- c
    conn <- file(fileName,open="r")
    linn <-readLines(conn)
    for (i in 1:length(linn))
    {
      if(!grepl("classmwheadline", linn[i]))
      {    
        if(linn[i]!=" " && linn[i]!="" & linn[i]!=" " && linn[i]!='')
        {
          stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
          stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
          doc = stringr::str_replace_all(linn[i], stopwords_regex, '')
          
          doc = gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ",doc)
          doc = gsub(to_rep," ", doc)
          doc = gsub("\\s+"," ",doc)
          doc = str_trim(doc, side = c("both"))
          
          if(doc!="")
          {
            Test3 <- append(Test3,doc)
          } 
        }
      }  
    }
    close(conn)
  }
  
  lent2 = length(Test3)
  print(lent2)
  
  
  path6 = "test/test3"
  filelist6 = list.files(path6, pattern = ".*.txt")

  Test4=c()
  for(y in filelist6)
  {
    # print(y)
    c = paste(path6,y,sep="/")
    
    fileName <- c
    conn <- file(fileName,open="r")
    linn <-readLines(conn)
    for (i in 1:length(linn))
    {
      if(!grepl("classmwheadline", linn[i]))
      {    
        if(linn[i]!=" " && linn[i]!="" & linn[i]!=" " && linn[i]!='')
        {
          stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
          stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
          doc = stringr::str_replace_all(linn[i], stopwords_regex, '')
          
          doc = gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ",doc)
          doc = gsub(to_rep," ", doc)
          doc = gsub("\\s+"," ",doc)
          doc = str_trim(doc, side = c("both"))
          
          if(doc!="")
          {
            Test4 <- append(Test4,doc)
          } 
        }
      }  
    }
    close(conn)
  }
  
  lent3 = length(Test4)
  print(lent3)
  
  
  path7 = "test/test4"
  filelist7 = list.files(path7, pattern = ".*.txt")
  
  Test5=c()
  for(y in filelist7)
  {
    # print(y)
    c = paste(path7,y,sep="/")
    
    fileName <- c
    conn <- file(fileName,open="r")
    linn <-readLines(conn)
    for (i in 1:length(linn))
    {
      if(!grepl("classmwheadline", linn[i]))
      {    
        if(linn[i]!=" " && linn[i]!="" & linn[i]!=" " && linn[i]!='')
        {
          stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
          stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
          doc = stringr::str_replace_all(linn[i], stopwords_regex, '')
          
          doc = gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ",doc)
          doc = gsub(to_rep," ", doc)
          doc = gsub("\\s+"," ",doc)
          doc = str_trim(doc, side = c("both"))
          
          if(doc!="")
          {
            Test5 <- append(Test5,doc)
          } 
        }
      }  
    }
    close(conn)
  }
  
  lent4 = length(Test5)
  print(lent4)
  
  
  Test <- append(Test1,Test2)
  Test <- append(Test,Test3)
  Test <- append(Test,Test4)
  Test <- append(Test,Test5)
  

  Test1 <- lexicalize(Test,lower=TRUE)
  labelt <- append(rep(0,lent),rep(1,lent1))
  labelt <- append(labelt,rep(2,lent2))
  labelt <- append(labelt,rep(3,lent3))
  labelt <- append(labelt,rep(4,lent4))
  
  
#-----------------------------------------------
  
len1 = length(A)
len2 = length(B)
len3 = length(C)
len4 = length(D)
len5 = length(E)


A1 <- append(A,B)
A1 <- append(A1,C)
A1 <- append(A1,D)
A1 <- append(A1,E)


num.topics <- 5


## Initialize the params

label <- append(rep(0,len1),rep(1,len2))
label <- append(label,rep(2,len3))
label <- append(label,rep(3,len4))
label <- append(label,rep(4,len5))

A2 <- lexicalize(A1,lower=TRUE)


# Taking words with count atleast 3
#wc <- word.counts(A2$documents)
#to.keep <- A2$vocab[word.counts(A2$documents, A2$vocab) >= 3]


# Lexicalize again with the new vocab
#A3 <- lexicalize(A1, lower=TRUE, vocab=to.keep)

#length(A3)


#write.table(sapply(A2$documents, length)==0, file = "len.txt", sep = " ")
#str(A3[2773])
#A3[2773]

#write.table(to.keep, file = "vocab.txt", sep = " ")

set.seed(8675309)
params <- sample(c(-1,1), num.topics, replace=TRUE)


result <- slda.em(documents=A2$documents,
                  K=num.topics,
                  vocab=A2$vocab,
                  num.e.iterations=10,
                  num.m.iterations=4,
                  alpha=1.0, eta=0.1,
                  label,
                  params,
                  variance=0.25,
                  lambda=1.0,
                  logistic=FALSE,
                  method="sLDA")


top.words <- top.topic.words(result$topics, 20, by.score=TRUE)
top.words

write.table(top.words, file = "top_words.txt", sep = " ")

#write.table(word.counts(A2$documents, vocab = A2$vocab), file = "count.txt", sep = " ")


predictions <- slda.predict(Test1$documents,
                            result$topics, 
                            result$model,
                            alpha = 1.0,
                            eta=0.1)


#write.table(result$topics, file = "res.txt", sep = " ")


print(paste0("Training Data Size :" , length(A2$documents)))
print(paste0("No of Labels :" , num.topics))
print(paste0("Test Data Size :" , length(Test)))

print(top.words)

#round(predictions)

a <- matrix(nrow = length(predictions), ncol=1)
x=0;
for(y in labelt)
{
  x=x+1
  a[x,1] = y
}

pre = cbind(round(predictions),a)

#head(pre)
#grid.raster(pre)
write.table(pre, file = "pred.txt", sep = " ")

count=0;

for(i in 1:length(predictions))
{
   if(pre[i,1]==pre[i,2])
   {
      count=count+1;
   }
}

count1=0;
count2=0;
count3=0;
count4=0;
count5=0;

for(i in 1:length(predictions))
{
  if(pre[i,2]==0)
  {
    if(pre[i,1]==0)
    {
      count1=count1+1;
    }
  }
}

for(i in 1:length(predictions))
{
  if(pre[i,2]==1)
  {
    if(pre[i,1]==1)
    {
      count2=count2+1;
    }
  }
}

for(i in 1:length(predictions))
{
  if(pre[i,2]==2)
  {
    if(pre[i,1]==2)
    {
      count3=count3+1;
    }
  }
}

for(i in 1:length(predictions))
{
  if(pre[i,2]==3)
  {
    if(pre[i,1]==3)
    {
      count4=count4+1;
    }
  }
}

for(i in 1:length(predictions))
{
  if(pre[i,2]==4)
  {
    if(pre[i,1]==4)
    {
      count5=count5+1;
    }
  }
}


print(count1/lent)
print(count2/lent1)
print(count3/lent2)
print(count4/lent3)
print(count5/lent4)


count
length(labelt)
acc = count/length(labelt)
print(acc)


