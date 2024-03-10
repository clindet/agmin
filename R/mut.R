#' Show bar plots of mutation data
#'
#' @examples
#' maf <- read.csv(system.file("extdata", "aml_maf.txt", package = "agmin"), sep = "\t")
#' print(vis_mutc_bar(maf, gene = "Hugo_Symbol", type = "Variant_Classification"))
#' @export
vis_mutc_bar <- function(maf, palt = NULL,
                        sample_id = "Tumor_Sample_Barcode", gene = "Hugo_Symbol",
                        type = "Variant_Classification") {
  colnames(maf)[which(colnames(maf) == sample_id)] <- "sample_id"
  colnames(maf)[which(colnames(maf) == gene)] <- "gene"
  colnames(maf)[which(colnames(maf) == type)] <- "type"

  result_frq <- NULL
  for(i in unique(maf$gene)) {
    maf_tmp <- maf[maf$gene == i,]
    sp_count <- sapply(maf_tmp$sample_id, function(k){
      maf_sp_tmp <- maf_tmp[maf_tmp$sample_id == k,]
      is_multi(maf_sp_tmp)
    })

    total_case <- as.data.frame(table(maf_tmp$type))
    for(j in unique(maf_tmp$type)) {
      maf_class_tmp <- maf_tmp[maf_tmp$type == j,]
      count <- length(unique(maf_class_tmp$sample_id))
      if (count == 0) {
        next
      }
      multi <- sum(unique(maf_class_tmp$sample_id) %in% names(sp_count)[sp_count])
      count <- count - multi
      result_tmp <- cbind(gene=i, type=j, count)
      result_frq <- rbind(result_frq, result_tmp)
    }
    multi <- sum(unique(maf_tmp$sample_id) %in% names(sp_count)[sp_count])
    if (multi != 0) {
      result_tmp <- cbind(gene=i, "multiple", multi)
      result_frq <- rbind(result_frq, result_tmp)
    }
  }

  result_frq <- as.data.frame(result_frq)
  result_frq$gene <-as.factor(result_frq$gene)

  if (is.null(palt)) {
    lev <- c("missense", "nonsense", "frameshift",
             "nonframeshift_deletion", "nonframeshift_insertion",
             "nonframeshift_substitution", "splice", "ITD/PTD", "multiple")
    palt <- c("#3987cc", "#ff7f0e", "#db3d3d", "#7F7F7F", "#8C564B", "#AACCAA", "#5E42F7", "#BC2E20", "#998199")
    names(palt) <- lev
  } else {
    lev <- names(palt)
  }
  palt <- palt[names(palt) %in% c(unique(maf$type), "multiple")]


  result_frq$type <- factor(result_frq$type, levels = rev(lev))
  result_frq$count <-as.numeric(result_frq$count)
  result_frq_fin <- NULL
  for(i in unique(result_frq$gene)) {
    result_tmp <- result_frq[result_frq$gene == i,]
    total <- sum(result_tmp$count)
    result_tmp <- cbind(result_tmp, total)
    result_frq_fin <- rbind(result_frq_fin, result_tmp)
  }
  result_frq_fin <- as.data.frame(result_frq_fin)

  p <- ggplot(result_frq_fin,
              aes(reorder(gene, -total), count, fill=type)) +
    geom_bar(stat="identity") + theme_classic() +
    scale_fill_manual(values=palt[lev])+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Gene") + ylab("Counts of samples") +
    guides(fill=guide_legend(title="Mutation types"))
  return(p)
}

is_multi <- function(dat.tmp, type = "type") {
  if (length(unique(dat.tmp[,type])) >= 2) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Show scatter plots: mutation counts and age
#'
#'@export
vis_mutc_scatter <- function(x, maf, age = "age", facet_var = "fusion_genes", fill = "fusion_genes",
                            sample_id = "Tumor_Sample_Barcode", gene = "Hugo_Symbol",
                            type = "Variant_Classification") {

  x_list <- clean_x_maf(x, maf, age, fill, sample_id, gene, type)
  x <- x_list[[1]]
  p <- do.call(ggscatter, list(x, x = age, y='mut.count.Freq',
                          color = fill, size = 1))
  p <- p + xlab(age) + ylab("Number of mutations") +
    geom_smooth(method = 'lm', formula = y~x, se = TRUE, show.legend = FALSE) +
    stat_cor(data = x, aes(label = paste(..r.label.., ..p.label.., sep = '~`,`~')),
             method = 'spearman',
             label.x.npc = 'left', label.y.npc = 'top', size = 4)
  if (facet_var != "" && facet_var %in% colnames(x)) {
    p <- p + eval(parse(text=sprintf("facet_wrap(~%s)", facet_var)))
  }
  return(p)
}

#' Show boxplot plots: mutation counts and groups
#'
#'@export
vis_mutc_boxplot <- function(x, maf, age = "age", facet_var = "fusion_genes", fill = "fusion_genes",
                            sample_id = "Tumor_Sample_Barcode", gene = "Hugo_Symbol",
                            type = "Variant_Classification") {
  x_list <- clean_x_maf(x, maf, age, fill, sample_id, gene, type)
  x <- x_list[[1]]
  p <- ggviolin(x, size = 0.5,
                x = fill, y = "mut.count.Freq",
                color = fill, fill = "white",
                alpha = 0.8,
                add = c("jitter", "boxplot"),
                error.plot = "crossbar", add.param = list(color = fill, alpha = 0.3),
                xlab = fill, ylab = "Mutation counts",
                main = "", outlier.shape = NULL,
                legend = "bottom")

  c_groups <- unique(x[,fill])
  if (length(c_groups) < 5) {
    c_groups <- combn(length(c_groups), 2)
    c_group_final <- list()
    for(i in 1:ncol(c_groups)) {
      c_group_final[[i]] <- c(c_groups[1,i], c_groups[2,i])
    }
    p <- p + stat_compare_means(label = "p.format", method = "wilcox.test",
                                comparisons = c_group_final, hide.ns = T)
  } else {
    p <- p + stat_compare_means(label = "p.format", method = "wilcox.test",
                                ref.group = ".all.", hide.ns = T)
  }
  if (facet_var != "" && facet_var %in% colnames(x)) {
    p <- p + eval(parse(text=sprintf("facet_wrap(~%s)", facet_var)))
  }
  return(p)
}

#' Percent barplot
#'
#' @export
vis_mutc_percent <- function(x, maf, age = "age", facet_var = "fusion_genes", fill = "fusion_genes",
                  palt = NULL,
                  sample_id = "Tumor_Sample_Barcode", gene = "Hugo_Symbol",
                  type = "Variant_Classification", logist_mutc = 4) {
  x_list <- clean_x_maf(x, maf, age, fill, sample_id, gene, type)
  x <- x_list[[1]]

  if (facet_var != "" && facet_var %in% colnames(x)) {
    p <- NULL
    for (i in levels(x[,facet_var])) {
      xtmp <- x[x[,facet_var] == i,]
      res <- logist_val(xtmp, logist_mutc)
      z_val <- res[[1]]
      p_val <- res[[2]]
      ptmp <- do.call(ggbarstats, list(data = xtmp, x = "mut.count.Freq.class", y = fill,
                                      results.subtitle = FALSE,
                                      title = sprintf("%s-%s: Z-value: %s; P-value: %s",
                                                      facet_var, i, z_val, p_val),
                                      ggplot.component = list(ggplot2::scale_x_discrete(guide =
                                     ggplot2::guide_axis(n.dodge = 2)))
      ))
      if (!is.null(palt)) {
        ptmp <- ptmp + scale_fill_manual(values = palt)
      }
      if (is.null(p)) {
        p <- ptmp
      } else {
        p <- p + ptmp
      }
    }
  } else {
    res <- logist_val(x)
    z_val <- res[[1]]
    p_val <- res[[2]]
    p <- do.call(ggbarstats, list(data = x, x = "mut.count.Freq.class", y = fill,
                                  results.subtitle = FALSE,
                                  title = sprintf("Z-value:%s; P-value: %s",
                                                  z_val, p_val),
                                  ggplot.component = list(ggplot2::scale_x_discrete(guide =
                                  ggplot2::guide_axis(n.dodge = 2)))
    ))
    if (!is.null(palt)) {
      p <- p + scale_fill_manual(values = palt)
    }
  }
  return(p)
}

#' Percent line plots
#'
#' @examples
#' maf <- read.csv(system.file("extdata", "aml_maf.txt", package = "agmin"), sep = "\t")
#' x <- read.csv(system.file("extdata", "aml_samples.txt", package = "agmin"), sep = "\t")
#' @export
vis_mut_line <- function(x, maf, age = "age", fill = "fusion_genes",
                         sample_id = "Tumor_Sample_Barcode", gene = "Hugo_Symbol",
                         type = "Variant_Classification") {
  if (is.null(fill) || fill == "" || !fill %in% colnames(x)) {
    fill <- "fill"
    x$fill <- "all"
  }
  x_list <- clean_x_maf(x, maf, age, fill, sample_id, gene, type)
  res <- x_list[[3]]
  p <- ggplot(data = res,
              aes(x = age_cut,
                  y = rate, color = fill, shape = fill,
                  group = fill)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = "Age groups", y = "Mutation rate (%)") +
    facet_wrap(~gene, ncol = 7) +
    theme_base()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(p)
}

clean_x_maf <- function (x, maf, age = "age", fill = "fusion_genes",
                         sample_id = "Tumor_Sample_Barcode", gene = "Hugo_Symbol",
                         type = "Variant_Classification") {
  if (is.null(fill) || fill == "" || !fill %in% colnames(x)) {
    x$fill <- "all"
    x$fill <- factor(x$fill)
    fill <- "fill"
  }

  x$age <- as.numeric(x$age)
  colnames(x)[which(colnames(maf) == age)] <- "age"
  colnames(x)[which(colnames(maf) == fill)] <- "fill"
  colnames(maf)[which(colnames(maf) == sample_id)] <- "sample_id"
  colnames(maf)[which(colnames(maf) == gene)] <- "gene"
  colnames(maf)[which(colnames(maf) == type)] <- "type"

  if (!is.factor(x[,fill]) && length(x[,fill]) != 1) {
    x[!is.na(x[,fill]) & x[,fill] != "." & x[,fill] != 0, fill] <- "yes"
    x[x[,fill] != "yes", fill] <- "no"
  }

  x <- data.frame(x, mut.count = table(maf$sample_id)[row.names(x)])
  x$mut.count.Freq[is.na(x$mut.count.Freq)] <- 0

  x$mut.count.Freq.class <- ""
  x$mut.count.Freq.class[x$mut.count.Freq == 0] <- "Negative"
  x$mut.count.Freq.class[x$mut.count.Freq == 1] <- "1"
  x$mut.count.Freq.class[x$mut.count.Freq %in% c(2, 3)] <- "2-3"
  x$mut.count.Freq.class[x$mut.count.Freq %in% c(4, 5)] <- "4-5"
  x$mut.count.Freq.class[x$mut.count.Freq %in% c(6, 10)] <- "6-10"
  x$mut.count.Freq.class[x$mut.count.Freq >10 ] <- "11+"
  x$mut.count.Freq.class <- factor(x$mut.count.Freq.class,
                                   levels = rev(c("Negative", "1", "2-3", "4-5", "6-10", "11+")))
  if (!is.factor(x[,fill]) && length(unique(x[,fill])) != 1) {
    x_r <- nrow(x)
    x <- rbind(x, x)
    x[(x_r+1):nrow(x),fill] <- "all"
  }

  pcent_res <- NULL
  median_age <- c()
  cor_age <- c()
  for(j in unique(maf[,"gene"])){
    median_age <- c(median_age,
                    median(x[row.names(x) %in% maf[maf[,"gene"] == j, "sample_id"],"age"], na.rm = TRUE))
    names(median_age)[length(median_age)] <- j
    x$cor_age <- as.numeric(row.names(x) %in% maf[maf[,"gene"] == j, "sample_id"])
    cor_age <- c(cor_age, cor.test(x$age, x$cor_age, method = "spearman")$estimate)
    names(cor_age)[length(cor_age)] <- j
    for(i in levels(x[,"age_cut"])) {
      for(k in unique(x[,fill])) {
        tmp <- x[x[,"age_cut"] == i & x[,fill] == k,]
        rate <- sum(row.names(tmp) %in% maf[maf[,"gene"] == j, "sample_id"]) / nrow(tmp) * 100
        tmp_stat <- data.frame(age_cut = i, gene = j, rate = rate, fill = k)
        pcent_res <- rbind(pcent_res, tmp_stat)
      }
    }
  }
  pcent_res$age_cut <- factor(pcent_res$age_cut, levels = levels(x[,"age_cut"]))
  gene_rank <- names(sort(median_age * cor_age, decreasing = TRUE))
  pcent_res$gene <- factor(pcent_res$gene, levels = gene_rank)

  return(list(x, maf, pcent_res))
}

logist_val <- function (x, logist_mutc = 4) {
  x2 <- x
  x2[,"age_cut"] <- as.numeric(x2[,"age_cut"])
  x2$logist_mutc <- 0
  x2$logist_mutc[x2$mut.count.Freq >= logist_mutc] <- 1
  logistic <- eval(parse(text = sprintf("glm(logist_mutc ~ %s,
                      data = x2, family = binomial())", fill)))
  res <- summary(logistic)
  z_val <- round(res$coefficients[2,3], digits = 3)
  p_val <- round(res$coefficients[2,4], digits = 3)
  return(list(z_val, p_val))
}
