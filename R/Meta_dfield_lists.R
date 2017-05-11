
# sample meta_dfield_list's

cran_dfield_list = list(
	"p" = c("package"),
	"d" = c("description", "descriptions"),
	"t" = c("title"),
	"v" = c("version"),
	"a" = c("author", "authors", "author@r", "authors@r"),
	"l" = c("license"),
	"m" = c("maintainer"),
	"ty"= c("type")
)


npm_dfield_list = list(
	"a" = c("author", "authors"),
	"d" = c("description", "descriptions"),
	"n" = c("name"),
	"v" = c("version"),
	"l" = c("license", "licenses"),
	"k" = c("keywords", "keyword"),
	"dep" = c("dependencies"),
	"m" = c("main")
)


php_dfield_list = list(
	"a" = c("author", "authors"),
	"d" = c("description", "descriptions"),
	"n" = c("name"),
	"r" = c("require"),
	"l" = c("license", "licenses"),
	"k" = c("keywords", "keyword")
)


gitenberg_dfield_list = list(
	"t" = c("title"),
	"d" = c("description", "descriptions"),
	"i" = c("identifiers"),
	"l" = c("language"),
	"p" = c("publisher"),
	"r" = c("rights"),
	"s" = c("subjects"),
	"c" = c("creator"),
	"typ" = c("gutenberg_type"),
	"iss" = c("gutenberg_issued")
)


qlet_dfield_list = list(
	# Style guide data fields : required
	"q" = c("quantletname", "qname", "nameofquantlet", "name_of_quantlet"),
	"p" = c("publishedin", "published"),
	"a" = c("author", "authors", "author[new]", "author[r]", "author[matlab]", "author[m]", "author[sas]"),
	"d" = c("description", "descriptions"),
	"k" = c("keywords", "keyword", "keywords[new]", "keyword[new]"),
	# Style guide data fields : optional
	"df"= c("datafile", "datafiles", "datafile[matlab]", "datafiles[matlab]", "datafile[r]", "datafile[m]", "datafile[example]"),
	"e" = c("example", "examples", "example[matlab]"),
	"i" = c("input", "inputs", "input[matlab]"),
	"o" = c("output", "outputs", "output[matlab]"),
	"s" = c("submitted", "submittedby", "submitted[r]", "submitted[matlab]", "submitted[sas]", "submitted[python]"),
	"sa"= c("seealso", "see", "seealso[matlab]"),
	# free data fields
	"ce" = c("codeeditor", "codeeditors"),
	"cp" = c("codeproblem", "codeproblems", "codeproblems[r]", "codeproblems[matlab]"),
	"cw" = c("codewarning", "codewarnings"),
	"od" = c("outdated"),
	"sf" = c("subfunction", "subfunctions"),
	"u"  = c("usage")
)
