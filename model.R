library(jsonlite)
options(timeout=45)
cutoff <- as.Date("2026-11-23")
codes <- c("688686","688003","688400","300802","300400")
names_cn <- c("奥普特","天准科技","凌云光","矩子科技","劲拓股份")
api <- function(report, filter="", size=100, page=1) {
  url <- paste0("https://datacenter-web.eastmoney.com/api/data/v1/get?reportName=",report,
                "&columns=ALL&sortColumns=REPORT_DATE&sortTypes=-1&pageSize=",size,
                "&pageNumber=",page,if(nzchar(filter)) paste0("&filter=",URLencode(filter,reserved=TRUE)))
  response <- fromJSON(paste(system2("curl",c("-fsSL","--retry","2",shQuote(url)),stdout=TRUE),collapse=""))
  stopifnot(response$success)
  response$result$data
}
refresh <- "--refresh" %in% commandArgs(trailingOnly=TRUE) ||
  !all(file.exists(c("financial_source.csv","industry_source.csv")))
if (refresh) {
finance <- do.call(rbind,lapply(codes,function(code) {
  x <- api("RPT_DMSK_FN_INCOME",sprintf('(SECURITY_CODE="%s")',code))
  x <- x[,c("SECURITY_CODE","REPORT_DATE","NOTICE_DATE","TOTAL_OPERATE_INCOME","OPERATE_COST","PARENT_NETPROFIT")]
  x$REPORT_DATE <- as.Date(x$REPORT_DATE)
  x$NOTICE_DATE <- as.Date(x$NOTICE_DATE)
  x <- x[x$NOTICE_DATE<=cutoff,]
  x <- x[order(x$REPORT_DATE,-as.numeric(x$NOTICE_DATE)),]
  x[!duplicated(x$REPORT_DATE),]
}))
write.csv(finance,"financial_source.csv",row.names=FALSE)
macro <- do.call(rbind,lapply(1:3,function(page) api("RPT_ECONOMY_INDUS_GROW",page=page)))
macro$month <- as.Date(macro$REPORT_DATE)
macro <- macro[macro$month<=as.Date("2026-10-01") & !duplicated(macro$month),]
write.csv(macro,"industry_source.csv",row.names=FALSE)
} else {
  finance <- read.csv("financial_source.csv", colClasses=c(SECURITY_CODE="character"))
  finance$REPORT_DATE <- as.Date(finance$REPORT_DATE)
  finance$NOTICE_DATE <- as.Date(finance$NOTICE_DATE)
  macro <- read.csv("industry_source.csv")
  macro$month <- as.Date(macro$month)
}
quarter_id <- function(d) as.integer(format(d,"%Y"))*4L+(as.integer(format(d,"%m"))-1L)%/%3L
panel <- do.call(rbind,lapply(split(finance,finance$SECURITY_CODE),function(x){
  id <- quarter_id(x$REPORT_DATE)
  pos <- match(id-1,id)
  fields <- c("TOTAL_OPERATE_INCOME","OPERATE_COST","PARENT_NETPROFIT")
  y <- x[,fields]
  for (field in fields) y[[field]] <- ifelse(id%%4==0,x[[field]],x[[field]]-x[[field]][pos])
  names(y) <- c("revenue","cost","profit")
  y$growth <- 100*(y$revenue/y$revenue[match(id-4,id)]-1)
  # The prior quarter's filing is not generally public at the opening of a quarter.
  y$lag2 <- y$growth[match(id-2,id)]
  y$code <- x$SECURITY_CODE
  y$q <- id
  y$period <- x$REPORT_DATE
  y$margin <- 100*(1-y$cost/y$revenue)
  y$net_margin <- 100*y$profit/y$revenue
  y
}))
# Use the last month of the quarter two quarters back. Its release is comfortably
# before forecast origin even with the usual next-month statistical release.
end_month <- as.integer(format(macro$month,"%m"))%%3==0
panel$industry <- macro$BASE_SAME[end_month][match(panel$q-2,quarter_id(macro$month[end_month]))]
panel <- panel[order(panel$q,panel$code),]
write.csv(panel,"quarter_panel.csv",row.names=FALSE)
d <- subset(panel,q>=2022*4 & complete.cases(growth,lag2,industry))
pred <- do.call(rbind,lapply(sort(unique(d$q[d$q>=2024*4])),function(q){
  # At the beginning of q, actual results through q-2 are normally available.
  train <- d[d$q<=q-2,]
  test <- d[d$q==q,]
  if(nrow(train)<25) return(NULL)
  test$carry <- test$lag2
  test$baseline <- predict(lm(growth~factor(code)+lag2,train),test)
  test$macro_model <- predict(lm(growth~factor(code)+lag2+industry,train),test)
  test
}))
stopifnot(is.data.frame(pred),nrow(pred)>0,all(is.finite(pred$macro_model)))
write.csv(pred,"backtest.csv",row.names=FALSE)
models <- c("carry","baseline","macro_model")
scores <- function(x) t(sapply(x[,models],function(p) c(MAE=mean(abs(x$growth-p)),RMSE=sqrt(mean((x$growth-p)^2)))))
print(scores(pred))
print(lapply(split(pred,pred$code),scores))
print(lapply(split(pred,pred$q%/%4),scores))
print(summary(lm(growth~factor(code)+lag2+industry,d))$coefficients)
ttm <- function(x){
  a <- tail(x,4); b <- head(tail(x,8),4)
  c(revenue=sum(a$revenue)/1e8,growth=100*(sum(a$revenue)/sum(b$revenue)-1),
    gross=100*(1-sum(a$cost)/sum(a$revenue)),profit=sum(a$profit)/1e8,
    net=100*sum(a$profit)/sum(a$revenue))
}
summary_table <- do.call(rbind,lapply(split(panel,panel$code),ttm))
print(summary_table)
x <- subset(panel,code=="688686" & q>=2022*4)
h <- t(sapply(8:nrow(x),function(i) ttm(x[seq_len(i),])))
h <- h[complete.cases(h),]
p <- c(.25,.5,.75)
assumptions <- sapply(c("growth","gross","net"),function(k) quantile(h[,k],p,names=FALSE,type=7))
# Keep current profitability as the neutral anchor and use the historical
# interquartile distances to represent downside and recovery.
for (k in c("gross","net")) {
  assumptions[,k] <- summary_table["688686",k]+assumptions[,k]-assumptions[2,k]
}
base <- tail(x,4)
out <- do.call(rbind,lapply(1:3,function(i) {
  a <- assumptions[i,]
  rev <- base$revenue/1e8*(1+a["growth"]/100)
  data.frame(scenario=c("bear","base","bull")[i],q=base$q+4,growth=a["growth"],
             gross_margin=a["gross"],net_margin=a["net"],revenue=rev,
             gross_profit=rev*a["gross"]/100,net_profit=rev*a["net"]/100,row.names=NULL)
}))
write.csv(out,"scenarios.csv",row.names=FALSE)
print(assumptions)
print(aggregate(cbind(revenue,gross_profit,net_profit)~scenario,out,sum))
cat("Panel rows:",nrow(panel)," Model rows:",nrow(d)," Backtest rows:",nrow(pred)," Historical trailing observations:",nrow(h),"\n")
pdf("backtest_error.pdf",width=8,height=4.2)
barplot(t(scores(pred)[,c("MAE","RMSE")]),beside=TRUE,col=c("#2b5c86","#df8c56"),
        names.arg=c("Carry forward","Company + lag","Company + lag + industry"),
        ylab="Forecast error (percentage points)",ylim=c(0,36),border=NA,
        legend.text=c("MAE","RMSE"),args.legend=list(x="topright",bty="n"))
invisible(dev.off())
