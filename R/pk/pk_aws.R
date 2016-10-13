library("aws.s3")

get_items = function(bucket = "bayesianexpdesign") {
  bucket = get_bucket(bucket = bucket)
  bucket_elements = unlist(lapply(bucket, `[[`, "Key"))
  lapply(bucket_elements, s3readRDS,  bucket = "bayesianexpdesign")
}

combine_items = function(bucket = "bayesianexpdesign") {
  l = get_items(bucket)
  dd = l[[1]]*0
  for(i in 1:length(l)) {
    dd_tmp = l[[i]]
    dd[,4] = (dd[,4]*dd[,3] + dd_tmp[,4]*dd_tmp[,3])/(dd[,3] + dd_tmp[,3])
    dd[,3] = dd_tmp[,3] + dd[,3]
    dd[is.nan(dd[,4]),4] = 0
    dd[,6] = pmax(dd[,6], dd_tmp[,6])
  }
  ## Set missing values to mean
  dd[dd[,6]==0,4] = sum((dd[,3] > 0.5)*dd[,4])/sum(dd[,3])
  dd[,1:2] = l[[1]][,1:2]
  return(dd)  
}

upload_item = function(m, bucket = "bayesianexpdesign") {
  hostname = as.vector(Sys.info()["nodename"])  
  object = paste0(hostname, ".RDS")
  s3saveRDS(m, bucket = bucket, object = object)
}

