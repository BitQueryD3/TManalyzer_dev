
save_JSON = function(pre_json, filename, showPretty = TRUE, utf8 = FALSE) {
	if (showPretty) { print(toJSON(pre_json, pretty = TRUE, auto_unbox = TRUE)) }
	
	jsonf = toJSON(pre_json, auto_unbox = TRUE)
	
	if (!utf8)
		{ cat(jsonf, file=filename)
	} else {
		# this saves the file in UTF-8 format
		writeLines(jsonf, filename, sep = "", useBytes = T)
	}
}

