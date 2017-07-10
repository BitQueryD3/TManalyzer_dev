
### k-means version

D3Visu_ClusterTopic_kmeans = function(A, k = 16, n = 5, mds_plot = TRUE, topic_print = TRUE, d_mat = NULL) {
	# k-means clustering:
	# clus = kmeans(A, k)
	clus = kmeans(A, k, iter.max = 50)
	print(sort(clus$size, decreasing=T))
	print(paste("totss:", clus$totss))
	print(paste("withinss:", clus$tot.withinss))
	
	if (mds_plot) {
		# MDS for 2 dimensions
		d         = dist(A)
		mdsAll    = cmdscale(d,k=2)

		plot(mdsAll,type="n",xlab="",ylab="",main="Metric MDS for k-means")
		text(mdsAll[,1],mdsAll[,2],"*", cex=0.8,col=clus$cluster)
		legend("bottomright", col= 1:max(clus$cluster), pch=1, legend = paste("Cluster", 1:max(clus$cluster), sep = ""), lty=3)
	}

	# Cluster-Centroids, possible interpretation of the cluster topic
	# Top n words from every cluster center as possible topic of the cluster
	cl_topic_vec = vector()
	for (i in 1:dim(clus$centers)[1]) {
		s = sort(clus$centers[i,], decreasing=T)
		cl_topic_vec[i] = paste(names(s)[1:n], collapse = ", ")
		if (topic_print) {
			cat(paste("cluster ", i, ": ", sep=""))
			cat(names(s)[1:n], "\n")
		}
	}
	
	if (class(d_mat) == "dist") {
		dev.new()
		si = silhouette(clus$cluster, d_mat)
		plot(si)
	}
	
	result_v = list()
	result_v$cl_topic_vec = cl_topic_vec
	result_v$cluster = clus$cluster
	result_v$centers = clus$centers
	return(result_v)
}



### PAM version

D3Visu_ClusterTopic_pam = function(A, k = 16, n = 5, mds_plot = TRUE, topic_print = TRUE, d_mat = NULL) {

	clus = pam(A, k)
	print(sort(clus$clusinfo[,"size"], decreasing=T))
	print(paste("clus.avg.widths:", paste(round(clus$silinfo$clus.avg.widths, 2), collapse = ", ")))
	print(paste("avg.width:", clus$silinfo$avg.width))

	if (mds_plot) {
		if (class(d_mat) == "dist") { d = d_mat } else { d = dist(A) }
		mdsAll    = cmdscale(d, k = 2)		# mds for 2 dimensions
		
		plot(mdsAll,type="n",xlab="",ylab="",main="Metric MDS for k-medoids")
		text(mdsAll[,1],mdsAll[,2],"*", cex=0.8,col=clus$clustering)
		legend("topright", col= 1:max(clus$clustering), pch=1, legend = paste("Cluster", 1:max(clus$clustering), sep = ""), lty=3)
	}
	
	# Cluster-Centroids, possible interpretation of the cluster topic
	# Top n words from every cluster medoid as possible topic of the cluster
	cl_topic_vec = vector()
	for (i in 1:k) {
		s = sort(clus$medoids[i,], decreasing=T)
		cl_topic_vec[i] = paste(names(s)[1:n], collapse = ", ")
		if (topic_print) {
			cat(paste("cluster ", i, ": ", sep=""))
			cat(names(s)[1:n], "\n")
		}
	}
	
	if (class(d_mat) == "dist") {
		dev.new()
		si = silhouette(clus$clustering, d_mat)
		plot(si)
	}
	
	result_v = list()
	result_v$cl_topic_vec = cl_topic_vec
	result_v$cluster = clus$clustering
	result_v$medoids = clus$medoids
	
	return(result_v)
}
