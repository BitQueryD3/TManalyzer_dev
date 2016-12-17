

yaml.list.extract = function(yaml_list, weight = NULL, keywords_spaces = TRUE) {
	t_vec = vector()
	conf  = names(weight)
  
	for (i in 1:length(yaml_list)) {
		info = ""
		yaml_meta = yaml_list[[i]]
		
		if (is.null(weight)) {
			# serializes the parsed yaml obj (associated list) to a big single string, for further TM analysis
			info = paste(unlist(yaml_meta), collapse = " ")
		} else {
		
			keywords = yaml.getQField(yaml_meta, "k")
			if (keywords_spaces) {
				keywords = gsub("-", " ", keywords)
			}
		
			if ("q" %in% conf) {info = paste(info, paste(rep(yaml.getQField(yaml_meta, "q"), weight[["q"]]), collapse = ", "))}
			if ("d" %in% conf) {info = paste(info, paste(rep(yaml.getQField(yaml_meta, "d"), weight[["d"]]), collapse = ", "))}
			if ("k" %in% conf) {info = paste(info, paste(rep(keywords, weight[["k"]]), collapse = ", "))}
			if ("a" %in% conf) {info = paste(info, paste(rep(yaml.getQField(yaml_meta, "a"), weight[["a"]]), collapse = ", "))}
			if ("df"%in% conf) {info = paste(info, paste(rep(yaml.getQField(yaml_meta, "df"), weight[["df"]]), collapse = ", "))}
			if ("sa" %in% conf){info = paste(info, paste(rep(yaml.getQField(yaml_meta, "sa"), weight[["sa"]]), collapse = ", "))}
			if ("s" %in% conf) {info = paste(info, paste(rep(yaml.getQField(yaml_meta, "s"), weight[["s"]]), collapse = ", "))}
			if ("e" %in% conf) {info = paste(info, paste(rep(yaml.getQField(yaml_meta, "e"), weight[["e"]]), collapse = ", "))}
			# raw meta info
			if ("p" %in% conf) {info = paste(info, paste(rep(yaml_meta$path, weight[["p"]]), collapse = ", "))}
		}
		t_vec = c(t_vec, info)
	}
	
	q_names = sapply( yaml_list, function(y){ yaml.getQField(y, "q") } )
	
	res = list()
	res$t_vec = t_vec
	res$q_names = q_names
	return(res)
}



# TM helping functions and definitions

qn_stopwords <- c("the", "a", "an", "and", "else", "also", "that", "if", "when",
				  "book", "input", "inputs", "quantlet", "quantnet", "example", "usage", "data", "plot", "plots", "plotted", 
				  "output", "description", "function", "functions", "author", "end", "quot",
				  "with", "from", "off", "for", "to", "in", "of", "as", "between", "by", "up",
				  "n", "m",
				  "they", "it", "all", "none", "some", "which", "this",
				  "are", "is", "was", "given", "hold", "close", "clear", "chosen", "provides", 
				  "refers", "required", "see", "show", "shows", "using", "used")
				  

cran_stopwords <- c("the", "a", "an", "and", "else", "also", "that", "if", "when",
				  "book", "input", "inputs", "quantlet", "quantnet", "example", "usage", "data", "plot", "plots", "plotted", 
				  "output", "description", "function", "functions", "author", "end", "quot",
				  "with", "from", "off", "for", "to", "in", "of", "as", "between", "by", "up",
				  "n", "m",
				  "they", "it", "all", "none", "some", "which", "this",
				  "are", "is", "was", "given", "hold", "close", "clear", "chosen", "provides", 
				  "refers", "required", "see", "show", "shows", "using", "used", "use",
				  "statist", "packag", "can", "method", "includ", "provid", "implement")

				  
cleanCorpus = function(corpus, stopwords_list_selected) {
	corpus.tmp <- tm_map(corpus, removePunctuation)
	corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
	corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
	corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
	corpus.tmp <- tm_map(corpus.tmp, stemDocument)
	corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords_list_selected)
	#corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
	return(corpus.tmp)
}


tm.create.models = function(meta_vec, stopwords_select = "qn", tf_trim = 2, tf_weight = "nnc",
							models = c("tt", "lsa", "lsa_spec"), lsa_dim = 50 ) {

	stopwords_list = list()
	stopwords_list$qn = qn_stopwords
	stopwords_list$cran = cran_stopwords
	
	stopwords_list_selected = stopwords_list[[stopwords_select]]
	
	t = VCorpus(VectorSource(meta_vec$t_vec))
	t.cl  = cleanCorpus(t, stopwords_list_selected)

	## Trimming and weighting
	# For Yaml: discard tf <= 2
	tdm_tf_trimmed = TermDocumentMatrix(t.cl, list(bounds = list(global = c(tf_trim + 1, Inf))))
	tdm_tf_trimmed_weighted = weightSMART(tdm_tf_trimmed, spec = tf_weight)

	## Models
	# Basis Model: BVSM
	m_a = as.matrix(tdm_tf_trimmed_weighted)
	colnames(m_a) = meta_vec$q_names
	print(paste("Dim TDM:", paste(dim(m_a), collapse = ",")))
	tm_res = list()
	tm_res$b = t(m_a)

	# Term-Term-Correlation Model
	if ("tt" %in% models) {
		m_tt = t(m_a) %*% m_a
		tm_res$tt = t(m_tt)
	}
	
	# LSA Model
	if ("lsa" %in% models) {
		space_auto = lsa(m_a)
		m_lsa      = as.textmatrix(space_auto)
		print(paste("Dim LSA Auto:", length(space_auto$sk)))
		tm_res$lsa = t(m_lsa)
		tm_res$lsa_space = space_auto
	}
	
	# special LSA , dim provided by lsa_dim
	if ("lsa_spec" %in% models) {
		space_dim  = lsa(m_a, dims = lsa_dim)
		m_lsa_dim  = as.textmatrix(space_dim)
		tm_res$lsa_spec = t(m_lsa_dim)
		tm_res$lsa_space_spec = space_dim
	}
	
	return(tm_res)
}


query.tm.fold_in = function(query, A_list, tf_weight = "nnc", latex_output = TRUE) {
	#tfidf normed: tf_weight = "ntc"
	
	m_a			= t(A_list$b)
	m_tt		= t(A_list$tt)
	m_lsa 		= t(A_list$lsa)
	m_lsa_spec	= t(A_list$lsa_spec)

	corp  = VCorpus(VectorSource(query))
	corp  = cleanCorpus(corp, qn_stopwords)
	tdm   = TermDocumentMatrix(corp, list(dictionary = rownames(m_a)))
	# tf weighting scheme
	tdm_weighted = weightSMART(tdm, spec = tf_weight)
	
	q_v				= as.matrix(tdm_weighted)
	q_v_tt			= t(m_a) %*% q_v
	q_v_lsa			= fold_in(q_v, A_list$lsa_space)
	q_v_lsa_spec	= fold_in(q_v, A_list$lsa_space_spec)
	
	tdm = TermDocumentMatrix(corp)
	q_m = as.matrix(tdm)
	colnames(q_m) = paste("q", colnames(q_m), sep = "")
	
	res = list()
	
	res$q_tm_list		= list(q_v, q_v_tt, q_v_lsa, q_v_lsa_spec)
	res$tm_models_list	= list(m_a, m_tt, m_lsa, m_lsa_spec)
	res$q_tdm = q_m
	res$query = query
	
	if (latex_output) {
		latex_out = print(xtable(q_m, align = paste("r", paste(rep("|c", length(query)), collapse = ""), sep = ""), digits = 0),
					 include.rownames = T, include.colnames = T)
		res$latex_out = latex_out
	}
	
	return(res)
}


q_tdm_sim.tm.list = function(query.tm.folded, sim_meth = "cosine", full_m = FALSE) {
	sim_tm_res		= list()
	q_tm_list 		= query.tm.folded$q_tm_list
	tm_models_list	= query.tm.folded$tm_models_list
	
	for (i in 1:length(q_tm_list)) {
		sim_tm_res[[i]] = simil(q_tm_list[[i]], tm_models_list[[i]], method = sim_meth, by_rows = FALSE, diag = full_m, upper = full_m)
	}
	
	res = list()
	res$query = query.tm.folded$query
	res$sim_tm_res = sim_tm_res
	
	return(res)
}


query.similar.doc.inspect = function(sim_tm_obj, sim_threshold = 0.7, digits_round = 2,
									 mini = FALSE, latex_output = FALSE, query_tm_text_tab = TRUE, false_hits = list()) {
	
	query = sim_tm_obj$query
	sim_tm_res = sim_tm_obj$sim_tm_res
	
	tm_labels = c("BVSM", "GVSM(TT)", "LSA", "LSA50")
	
	res_list = list()
	retrieved_m = matrix(0, length(query), length(sim_tm_res))
	precision_m = matrix(0, length(query), length(sim_tm_res))
	# i iter over TM models
	for (i in 1:length(sim_tm_res)) {
		res_q_list = list()
		q_tdm_sim = sim_tm_res[[i]]
		# k iter over TM queries
		for (k in 1:nrow(q_tdm_sim)) {
			res_q_row = round(sort(q_tdm_sim[k,][q_tdm_sim[k,] >= sim_threshold], decreasing = TRUE), digits_round)
			if (mini) {
				if (length(res_q_row) > 0) {
					res_q_list[[query[k]]] = res_q_row
				}
			} else {
				res_q_list[[query[k]]] = res_q_row
				retrieved_m[k, i] = length(res_q_row)
				# precision
				check_non_rel = names(res_q_row) %in% false_hits[[query[k]]]
				b_non_rel = length(check_non_rel[check_non_rel == TRUE])
				precision_m[k, i] = retrieved_m[k, i] - b_non_rel
			}
			
		}
		res_list[[i]] = res_q_list
	}
	
	
	query_tm_text = rep("", length(query))
	names(query_tm_text) = query
	
	query_tm_list = list()
	
	if (query_tm_text_tab) {
		for (i in 1:length(res_list)) {
			#print(paste("Model:", i))
			for (q_str in names(res_list[[i]])) {
				#print(q_str)
				#print(res_list[[i]][[q_str]])
				q_doc_vec = res_list[[i]][[q_str]]
				if (length(q_doc_vec) > 0) {
					q_doc_str = paste(paste(names(q_doc_vec), " (", q_doc_vec, ")", sep = ""), collapse = ", ")
				} else { q_doc_str = "no hits"}
			
				query_tm_text[q_str] = paste(query_tm_text[q_str], tm_labels[i], ": ", q_doc_str, "\\", sep = "")
				query_tm_list[[q_str]] = unique(c(query_tm_list[[q_str]], names(q_doc_vec)))
			}
		}
	}
	
	res = list()
	res$res_list = res_list
	res$query_tm_text = query_tm_text
	res$query_tm_list = query_tm_list
	
	if (!mini) {
	
		r_m = retrieved_m
		prec_m = precision_m
		colnames(r_m) = colnames(prec_m) = tm_labels
		rownames(r_m) = rownames(prec_m) = query
		
		res$retrieved_m = r_m
		res$precision_m = prec_m
	
		if (latex_output) {

			latex_out = print(xtable(r_m, align = paste("r", paste(rep("|c", length(tm_labels)), collapse = ""), sep = ""), digits = 0),
							  include.rownames = T, include.colnames = T)
			res$latex_out = latex_out	
		}
	}
	
	return(res)
}




