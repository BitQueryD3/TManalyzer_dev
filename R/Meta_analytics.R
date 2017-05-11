
# Meta analysis functions

meta.getDField = function(meta, meta_dfield_list, field_name) {
	accepted.fieldnames = meta_dfield_list[[field_name]]

	names(meta) = tolower(str_replace_all(names(meta), "[[:space:]]", ""))
	aname = vector()

	for (dname in names(meta)) {
		if (dname %in% accepted.fieldnames) {
			aname = c(aname, meta[[dname]])
		}
	}

	return(paste(aname, collapse = ", "))
}


DFields.from.meta = function(meta, meta_dfield_list) {
	meta_names_clean = tolower(str_replace_all(names(meta), "[[:space:]]", ""))
	found_dnames = vector()
	new_dnames = vector()

	for (dname in names(meta_dfield_list)) {
		if (any(meta_names_clean %in% meta_dfield_list[[dname]])) {
			found_dnames = c(found_dnames, dname)
		}
	}
	new_dnames = meta_names_clean[!(meta_names_clean %in% unlist(meta_dfield_list))]

	res = list()
	res$found_dnames = found_dnames
	res$new_dnames   = new_dnames

	return(res)
}


DFields.nchar.from.meta = function(meta, meta_dfield_list) {
	meta_names_clean = tolower(str_replace_all(names(meta), "[[:space:]]", ""))
	names(meta) = meta_names_clean

	n = length(meta_dfield_list)
	dfields_char = rep(0, n)
	names(dfields_char) = names(meta_dfield_list)

	for (dname in names(meta_dfield_list)) {
		#print(dname)
		for ( meta_dname in meta_names_clean[(meta_names_clean %in% meta_dfield_list[[dname]])] ) {
			#print(meta_dname)
			if ( is.null(meta[[meta_dname]]) ) {
				nchar_res = 0
			} else {
				cur_dfield = meta[[meta_dname]]
				# unlist possible structured data fields
				nchar_res = sum(nchar(unlist(cur_dfield)))
			}
			dfields_char[dname] = dfields_char[dname] + nchar_res
		}
	}

	return(dfields_char)
}


metadata.analysis = function(Metainfos, meta_dfield_list = NULL, rescale = 1) {
	meta_names = unlist(sapply( Metainfos, function(meta){ names(meta) } ))
	meta_names_distribution = sort(table(meta_names), decreasing = T)
	
	if (!is.null(meta_dfield_list)) {
		d_names  = unlist(sapply( Metainfos, function(meta){ DFields.from.meta(meta, meta_dfield_list)$found_dnames } ))
		d_names_distr = sort(table(d_names), decreasing = T)
		
		d_names_new  = unlist(sapply( Metainfos, function(meta){ DFields.from.meta(meta, meta_dfield_list)$new_dnames } ))
		d_names_new_distr = sort(table(d_names_new), decreasing = T)
		
		dfield_char_sums = rowSums(sapply( Metainfos, function(meta){ DFields.nchar.from.meta(meta, meta_dfield_list) } ))
		if (rescale != 1 & rescale > 0) {
			dfield_char_sums = dfield_char_sums / rescale
		}
	}
	
	res = list()
	res$meta_names_distribution = meta_names_distribution
	if (!is.null(meta_dfield_list)) {
		res$meta_dfield_list = meta_dfield_list
		res$d_names_distr = d_names_distr
		res$d_names_new_distr = d_names_new_distr
		res$dfield_char_sums = dfield_char_sums
	}
	
	return(res)
}


meta_dfield.projection = function(Metainfos, meta_dfield_list, dfields = NULL) {
	res_vec = list()
	
	dfields = unique(dfields)
	if (is.null(dfields)) { dfields = names(meta_dfield_list) }
	
	if ( !all(dfields %in% names(meta_dfield_list)) ) {
		stop("'dfields' entries must be a subset of 'names(meta_dfield_list)'!")
	}

	for (dname in dfields) {
		df_vec	= sapply( Metainfos, function(meta){ meta.getDField(meta, meta_dfield_list, dname) } )
		# res_vec[dname] = list(df_vec)
		res_vec[[dname]] = df_vec
	}
	return(res_vec)
}

