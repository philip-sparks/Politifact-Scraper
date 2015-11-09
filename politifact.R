library(XML)
library(stringr)
library(RColorBrewer)
library(plyr)

page_one_of <- str_extract_all(as(htmlParse("http://www.politifact.com/truth-o-meter/statements/"),"character"),"Page\\s+1\\s+of\\s+[[:digit:]]+")
total_pages <- as.numeric(str_extract(page_one_of,"[[:digit:]]+$"))

statements <- list()
for(ii in 1:total_pages)
{
  cat("\r",ii)
  base.url <- paste0("http://www.politifact.com/truth-o-meter/statements/?page=",ii)
  page <- htmlParse(base.url)
  links <- str_extract_all(toString.XMLNode(page),'truth-o-meter/statements/[[:digit:]]+/[^\\"]+')
  links <- links[[1]]
  links <- links[!duplicated(links)]
  links <- paste0("http://www.politifact.com/",links)
  statements <- c(statements, list(links))
  Sys.sleep(0.5)
}
statement.urls <- do.call(c,statements)

getFib <- function(url)
{
  single.url <- url
  page <- htmlParse(single.url)
  statement <- xpathSApply(page, "//*/div[@class='statement__text']", xmlValue)
  statement <- gsub('\\\"|\\\r|\\\n|\\\t', "", statement)
  
  subjects <- str_extract_all(toString.XMLNode(page),'\\<a href="/subjects/[^/]*')[[1]]
  subjects <- subjects[!grepl("By subject", subjects)]
  subjects <- unlist(str_extract_all(subjects, "[^/]+$"))
  subjects <- paste(subjects,collapse=", ")
  
  validity <- str_extract_all(toString.XMLNode(page), 'alt="Mostly False"|alt="Mostly True"|alt="True"|alt="Half-True"|alt="False"|alt="Pants on Fire!"')[[1]]
  validity <- gsub('^[^\\\"]+|\\\"',"",validity)
  
  who.when <- xpathSApply(page, "//*/p[@class='statement__meta']", xmlValue)[[1]]
  when <- str_extract(who.when,"(Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday), [[:alpha:]]+ [[:alnum:]]+, [[:digit:]]+")
  
  who.when <- gsub(" on ", "~", who.when)
  who <- str_extract(who.when,"^[^\\~]+")
  who <- str_extract(who,"\\s.+$")
  who <- gsub("^\\s+","",who)
  
  temp.fib <- data.frame("who"=who,"when"=when,"validity"=validity,"subjects"=subjects,"statement"=statement)
  Sys.sleep(runif(1,0.25,0.75))
  return(temp.fib)
}

all.statements <- lapply(statement.urls, FUN=function(x)try(getFib(x)))
all.statements.df <- do.call(rbind, all.statements)

gz <- gzfile("politifact-statements.csv.gz", "w")
write.csv(all.statements.df,gz, row.names=F)
close(gz)

fib.order <- c("Pants on Fire!","False","Mostly False","Half-True","Mostly True","True")

# POLITICIANS
##############################################################################################################
lie.table <- data.frame(table(all.statements.df$who, all.statements.df$validity)[,fib.order])
lie.table$total <- rowSums(lie.table)
lie.table[,1:6] <- lie.table[,1:6]/lie.table$total
lie.table <- lie.table[rev(order(lie.table$total)),]

lie.table$slope <- NA
lie.table$slope <- rowSums(lie.table[,4:6])/(rowSums(lie.table[,1:3]) + rowSums(lie.table[,4:6]))
# for(rr in 1:nrow(lie.table))
# {
#   temp.row <- data.frame("y"=unlist(lie.table[rr,1:6]),"x"=1:6)
#   mod <- lm(y~x, temp.row)
#   slope <- coef(mod)[2]
#   lie.table$slope[rr] <- slope
# }

lie.table <- lie.table[rev(order(lie.table$slope)),]
lie.table.pop <- lie.table[lie.table$total>19,]

fib.color <- brewer.pal(8,"RdYlBu")

png("top.liars.png", width=600, height=4800)
par(mfrow=c(nrow(lie.table.pop),1), mar=c(4,3,3,1))
for(ii in 1:nrow(lie.table.pop))
{
  barplot(unlist(lie.table.pop[ii,1:6]*lie.table.pop$total[ii]), main=row.names(lie.table.pop[ii,]), las=1, col=fib.color[2:7], border=fib.color[c(1:3,6:8)], names.arg=fib.order,axisnames=T)
}
dev.off()
##############################################################################################################


# SUBJECTS
##############################################################################################################
subjects <- sort(unique(str_split(paste(all.statements.df$subject,collapse=", "),", ")[[1]]))
subject.list <- list()
for(ss in subjects)
{
  temp.df <- all.statements.df[grepl(ss,all.statements.df$subject,ignore.case=T),]
  results <- table(temp.df$validity)[fib.order]
  results['subject'] <- ss
  subject.list <- c(subject.list,list(as.data.frame(t(as.matrix(results)))))
}
subject.df <- do.call(rbind.fill,subject.list)
subject.df[,1:6] <- apply(subject.df[,1:6],2,as.numeric)
subject.df$total <- rowSums(subject.df[,1:6])
subject.df[,1:6] <- subject.df[,1:6]/subject.df$total

subject.df$slope <- NA
subject.df$slope <- rowSums(subject.df[,4:6])/(rowSums(subject.df[,1:3]) + rowSums(subject.df[,4:6]))
head(subject.df)
# for(rr in 1:nrow(subject.df))
# {
#   temp.row <- data.frame("y"=unlist(subject.df[rr,1:6]),"x"=1:6)
#   mod <- lm(y~x, temp.row)
#   slope <- coef(mod)[2]
#   subject.df$slope[rr] <- slope
# }

subject.df <- subject.df[rev(order(subject.df$slope)),]
subject.df.pop <- subject.df[subject.df$total>9,]
head(subject.df.pop)

png("subject.lies.png", width=600, height=20000)
par(mfrow=c(nrow(subject.df.pop),1), mar=c(4,3,3,1))
for(ii in 1:nrow(subject.df.pop))
{
  barplot(unlist(subject.df.pop[ii,1:6]*subject.df.pop$total[ii]), main=subject.df.pop$subject[ii], las=1, col=fib.color[2:7], border=fib.color[c(1:3,6:8)], names.arg=fib.order,axisnames=T)
}
dev.off()