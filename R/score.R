
#' GSEA for SenMayo geneset
#'
#' @return list
#' @export
#'
#' @examples
#' data(geneList, package="DOSE")
#' gene <- geneList[abs(geneList) > 2]
#' res <- gsea_senmayo(gene,'Geneid',pvalueCutoff=1, minGSSize =1) # for all result (no-sign result include)
#' @importFrom clusterProfiler GSEA enricher
gsea_senmayo <- function(geneList,type = 'Geneid',...){
  if (type == 'Geneid') {
    res <- GSEA(geneList = geneList,TERM2GENE = SenMayoGene,...)
  } else if(type == 'Name') {
    res <- GSEA(geneList = geneList,TERM2NAME = SenMayoName,...)
  } else {
    stop('Not support Gene Type')
  }
  return(res)
}



#' GSVA (orssGSEA) for SenMayo geneset for each cell or sample
#' obj sample x gene
#'
#' @return list
#' @export
#' @examples
#' tpm_sample <- data(tpm_sample)
#' gsva_senmayo(t(tpm_sample),groups = 10, cores = 2)
#' @importFrom escape enrichIt
gsva_senmayo <- function(obj,...){
  res <- enrichIt(obj = obj, gene.sets = SenMayoSet,...)
  return(res)
}
