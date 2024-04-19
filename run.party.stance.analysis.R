folder.path <- "C:\\Users\\erezm\\coding\\stat215b\\final_project\\"
source(paste0(folder.path, "stat215b\\party.stance.R"))
#
load(paste0(folder.path, "data.Robj"))
data %<>% enrich.party.stance
data %<>% filter.party.stance
data %>% analyze.placement.within.party(fdr.q=0.05, counterfactual.alpha=0.05)
data %>% analyze.opposide.side.perception(fdr.q=0.05, bt.num.iterations=2000)
data %>% predict.party.alignment(train.frac=0.5, nfolds=5, fdr.q=0.05)
