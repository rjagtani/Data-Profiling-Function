data_profile=function (data,top_n=5, numeric.cutoff = -1,datetime_format=NULL,numerical_cols=NULL,categorical_cols=NULL,datetime_cols=NULL) 
{
  data=as.data.frame(data)
  if(length(numerical_cols)>0)
  {
    data[,numerical_cols]=lapply(as.data.frame(data[,numerical_cols]),function(x) as.numeric(as.character(x)))
  }
  if(length(categorical_cols)>0)
  {
    data[,categorical_cols]=lapply(as.data.frame(data[,categorical_cols]),as.character)
  }
  if(length(datetime_cols)>0 & !is.null(datetime_format))
  {
    data[,datetime_cols]=lapply(as.data.frame(data[,datetime_cols]),function(x) as.Date(as.character(x),format=datetime_format))
  }
  if(!is.null(datetime_format))
  {
    
    check_date_col=lapply(data,function(x) try(as.Date(x[!is.na(x)],format=datetime_format),silent = T))
    date_cols=names(unlist(lapply(check_date_col,function(x) if(class(x)!='try-error'){if(sum(!is.na(x))>0) {1}})))
    date_cols=date_cols[!date_cols %in% datetime_cols]
    data[,date_cols]=lapply(as.data.frame(data[,date_cols]),function(x) as.Date(as.character(x),format=datetime_format))
  } 
  cols <- 1:ncol(data)
  cats <- sapply(cols, function(i) is.factor(data[, i]) || 
                   is.character(data[, i]) ||(!inherits(data[,i], 'Date') & (!names(data)[i] %in% numerical_cols) & length(unique(data[, i])) <= 
                                                numeric.cutoff))
  cats <- which(cats == TRUE)
  nums <- sapply(cols, function(i) (is.numeric(data[, i]) & 
                                      length(unique(data[, i])) > numeric.cutoff) || names(data)[i] %in% numerical_cols)
  nums <- which(nums == TRUE)
  date_column <- sapply(cols, function(i) inherits(data[,i], 'Date'))
  date_column = which(date_column==TRUE)
  maxNA <- function(x) {
    if (all(is.na(x))) {
      return(NA)
    }
    else return(max(x, na.rm = TRUE))
  }
  minNA <- function(x) {
    if (all(is.na(x))) {
      return(NA)
    }
    else return(min(x, na.rm = TRUE))
  }
  if (length(nums) > 0) {
    num.data <- as.data.frame(data[, nums])
    n.miss <- colSums(is.na(num.data))
    n.miss.percent <- 100 * n.miss/nrow(num.data)
    n.unique <- lapply(num.data, unique)
    n.unique <- simplify2array(lapply(n.unique, length))
    n.mean <- apply(num.data, 2, mean, na.rm = TRUE)
    n.min <- apply(num.data, 2, minNA)
    n.max <- apply(num.data, 2, maxNA)
    n.quant <- apply(num.data, 2, quantile, probs = c(0.01, 
                                                      0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), na.rm = TRUE)
    type=rep("numerical",length(nums))
    col_name=names(data)[nums]
    col_number=nums
    count=nrow(data)
    n.output <- rbind(n.miss, n.miss.percent, 
                      n.unique, n.mean, n.min, n.quant, n.max)
    n.output <- data.frame(t(n.output))
    n.output <- round(n.output, 2)
    n.output=cbind(type,col_name,col_number,count,n.output)
    names(n.output) <- c(#"non-missing",
      "type","col_name","col_number","count","missing", "missing percent", 
      "unique_entries", "mean", "minimum", "1st percentile", "5th percentile", "10th percentile", "25th percentile", 
      "50th percentile", "75th percentile", "90th percentile", "95th percentile", "99th percentile", "maximum")
    row.names(n.output)=1:nrow(n.output)
    write.csv(n.output,paste0(getwd(),'/df_summ_num.csv'),row.names = T)
  }
  if (length(cats) > 0) {
    cat.data <- as.data.frame(data[, cats])
    cat.data[, 1:ncol(cat.data)] <- lapply(as.data.frame(cat.data), 
                                           as.character)
    cat.data[, 1:ncol(cat.data)] <- lapply(as.data.frame(cat.data), 
                                           function(x) {
                                             ifelse(x == "", NA, x)
                                           })
    n.miss <- colSums(is.na(cat.data))
    n.miss.percent <- 100 * n.miss/nrow(cat.data)
    n.unique <- lapply(cat.data, unique)
    n.unique <- simplify2array(lapply(n.unique, length))
    type=rep('categorical',length(cats))
    col_name=names(data)[cats]
    col_number=cats
    count=nrow(data)
    n.output <- rbind(n.miss, n.miss.percent, 
                      n.unique)
    n.output <- data.frame(t(n.output))
    n.output <- round(n.output, 2)
    n.categories <- lapply(cat.data, function(x) sort(table(x), decreasing = TRUE))
    max.cat <- max(unlist(lapply(n.categories, length)))
    if (max.cat > top_n) 
      max.cat <- top_n
    cat.names <- paste(rep(c("value", "freq"), max.cat), rep(1:max.cat, 
                                                             each = 2), sep = "_")
    n.output[, cat.names] <- ""
    n.output <- lapply(row.names(n.output), function(x) {
      tmp <- n.output[row.names(n.output) == x, ]
      freqs <- length(n.categories[[x]])
      freqs <- pmin(top_n, freqs)
      if (length(freqs) == 1 & freqs[1] == 0) {
        tmp[, paste("value", 1:max.cat, sep = "_")] <- NA
        tmp[, paste("freq", 1:max.cat, sep = "_")] <- NA
      }
      else {
        tmp[, paste("value", 1:freqs, sep = "_")] <- names(n.categories[[x]])[1:freqs]
        tmp[, paste("freq", 1:freqs, sep = "_")] <- unclass(n.categories[[x]])[1:freqs]
      }
      return(tmp)
    })
    n.output <- data.frame(do.call("rbind", n.output))
    count_of_others=integer()
    for(i in 1:nrow(n.output))
    {
      
      count_of_others[i]=nrow(data)-sum(unclass(n.categories[[row.names(n.output)[i]]])[1:top_n],na.rm=T)
    }
    n.output=cbind(type,col_name,col_number,count,n.output,count_of_others)
    n.output=n.output[,c(1:7,ncol(n.output),8:(ncol(n.output)-1))]
    names(n.output)[1:8]=c("type","col_name","col_number","count","missing","missing percent","unique entries","count of others")
    row.names(n.output)=1:nrow(n.output)
    write.csv(n.output,paste0(getwd(),'/df_summ_cat.csv'),row.names = TRUE)
  }
  
  if(length(date_column)>0)
  {
    date.data=as.data.frame(data[,date_column])
    type=rep('date',length(date_column))
    col_name=names(data)[date_column]
    col_number=date_column
    count=nrow(data)
    n.miss <- colSums(is.na(date.data))
    n.miss.percent <- 100 * n.miss/nrow(date.data)
    n.unique <- lapply(date.data, unique)
    n.unique <- simplify2array(lapply(n.unique,length))
    n.min <- apply(date.data, 2, minNA)
    n.max <- apply(date.data, 2, maxNA)
    n.output <- rbind(n.miss, n.miss.percent, 
                      n.unique)
    n.output <- data.frame(t(n.output))
    n.output=cbind(type,col_name,col_number,count,n.output,n.min,n.max)
    names(n.output) <- c(#"non-missing",
      "type","col_name","col_number","count","missing", "missing percent", 
      "unique_entries", "range from","range to")
    row.names(n.output)=1:nrow(n.output)
    write.csv(n.output,paste0(getwd(),'/df_summ_date.csv'),row.names = TRUE)
  }
}




