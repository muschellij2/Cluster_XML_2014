rm(list=ls())
library(XML)
library(stringr)
library(zoo)
library(plyr)
# setwd("~/Desktop")

root.url = "http://www.cdc.gov/"
main.url = file.path(root.url, 
	"vaccines/programs/vfc/awardees/vaccine-management/price-list/archive.html")
main.doc = htmlParse(main.url, isURL = TRUE)

ulist = xpathSApply(main.doc, "//a", xmlGetAttr, "href")
ulist = grep("price-list/2", ulist, value=TRUE)

all.urls = paste0(root.url, ulist)
dates = gsub(".*/(.*)[.]html", "\\1", ulist)
url = "http://www.cdc.gov//vaccines/programs/vfc/awardees/vaccine-management/price-list/2012/2012-02-01.html"
table.num = 1

get.tab = function(url, table.num = 1) {
	
	doc = htmlParse(url, isURL = TRUE)
	info = xpathSApply(doc, "//p", xmlValue)
	info = grep("^Prices last reviewed/updated: ", info, value=TRUE)
	info = gsub("Prices last reviewed/updated: ", "", info)
	stopifnot(length(info) == 1)
	tab = getNodeSet(doc, "//table")
	ped.tab = tab[[table.num]]
	ped.tab = getNodeSet(ped.tab, "//tbody")[[table.num]]
	trs = getNodeSet(ped.tab, paste0("//table[", 
		table.num, "]//tbody/tr"))

	##################
	# Parsing into a list structure
	##################
	rows = lapply(trs, xmlChildren)

	##################
	# Subsetting relevant data
	##################
	th = lapply(rows, function(x) {
		ind = which(names(x) %in% c("th", "td"))
		x[ind]
	})

	##################
	# Getting actual table data
	##################
	vals = lapply(th, function(x) {
		lapply(x, xmlValue)
	})

	# x = vals[[3]]
	# repper = rs[[3]]

	# x = th[[35]]
	##################
	# Getting the index for the data
	##################
	i = 0
	x =th[[i+1]]
	#####################
	# If data is th or td, then put the qualifiers for which column
	# this is unique to the CDC data
	#####################
	hdrs = lapply(th, function(x) {
		xx = lapply(x, xmlGetAttr, "headers")
		ids = lapply(x, xmlGetAttr, "id")
		ids = lapply(ids, gsub, pattern = "r.*$", 
			replacement = "")
		vals = paste0("t1c", 1:length(xx))	
		vals = mapply(function(x, y, z){
			if (is.null(x)){
				x = y
			}
			if (!is.null(z)){
				z = gsub('r.*', "", z)
			}
			if (is.null(x) | length(x) == 0){
				x = z
			}
			if (!grepl(paste0("t", table.num, "c"), x)){
				x = z
			}
			xss = strsplit(x, " ")[[1]]
			x = xss[length(xss)]
			return(x)
		}, xx, ids, vals)
		colnum = as.numeric(gsub(
			paste0("t", table.num, "c"), "", vals))
		i <<- i + 1
		# print(i)
		return(colnum)
	})

	##################
	# Fill in the data
	##################
	all.vals = t(mapply(function(x, ind){
		# print(x)
		x = unlist(x)
		z = rep(NA, length = 9)
		stopifnot(length(ind) == length(x))
		z[ind] = x
		return(z)
	}, vals, hdrs) )

	##################
	# Get header row
	##################
	hdr = all.vals[1,]
	hdr = gsub("/ ", "/", hdr)
	all.vals = all.vals[-1,]
	all.vals= as.data.frame(all.vals, stringsAsFactors = FALSE)

	##################
	# Cleaning up columns
	##################
	for (icol in seq(ncol(all.vals))){
		x = all.vals[, icol]
		x = str_trim(x)
		x = gsub("(.*)\r.*", "\\1", x)
		x = str_trim(x)
		### carry las observation forward
		x = na.locf(x)
		all.vals[, icol] = x
	}
	colnames(all.vals) = hdr
	all.vals$date = info
	return(all.vals)
}

########################
# Process data
########################
all.data = llply(all.urls, function(x) {
	# print(x)
	get.tab(x)
}, .progress = "text")

########################
# Fix the date of the price on there
########################
# end.data = mapply(function(x, y) {
# 	x$date =y
# 	x
# }, all.data, dates, SIMPLIFY = FALSE)

end.data = all.data

########################
# 
eg = expand.grid(seq(end.data), seq(end.data))
cn = lapply(end.data, function(x) {
	d = unique(colnames(x))
	d = d[!is.na(d)]
})
fcn = unique(unlist(cn))

end.data = lapply(end.data, function(x){
	cn = colnames(x)
	not.in = fcn[ !fcn %in% cn]
	for (i in seq_along(not.in)){
		x[, not.in[i]] = NA
	}
	x[, fcn]
})

end.data = do.call("rbind", end.data)

save(end.data, file="CDC_Vaccine_Data.Rda")