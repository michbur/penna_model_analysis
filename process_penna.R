# instalacja pakietow w R
# install.packages(c("WriteXLS", "dplyr", "reshape2"), repos = "https://cloud.r-project.org/")

# uruchamianie programu w bashu (w katalogu, gdzie sa powtorzenia)
# R -e 'source("process_penna.R")'

library(dplyr)
library(WriteXLS)
library(reshape2)

safe_as_numeric <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  x_num[is.na(x_num)] <- -1
  x_num
}

dir_name <- last(strsplit(getwd(), "/")[[1]])
replicate_list <- list.files()[!grepl(".", list.files(), fixed = TRUE)]



# population analysis - specimen demography --------------------------------

res_work <- do.call(rbind, lapply(replicate_list, function(ith_replicate) {
  all_files <- list.files(paste0(ith_replicate, "/"))
  age_files <- all_files[grepl(".txt", all_files, fixed = TRUE) & grepl("sc_age", all_files)]
  
  
  age_dat <- cbind(replicate = ith_replicate, 
                   do.call(cbind, lapply(age_files, function(ith_file) {
                     res <- read.table(paste0(ith_replicate, "/", ith_file)) %>% 
                       slice(76L:nrow(.)) %>% 
                       select(V1, V2)
                     
                     colnames(res) <- paste0(strsplit(ith_file, ".", fixed = TRUE)[[1]][1], 1L:2)
                     
                     res
                   })))
  
}))

all_steps <- strsplit(colnames(res_work), "age") %>% 
  sapply(last) %>% 
  strsplit("a") %>% 
  sapply(first) %>% 
  safe_as_numeric()


main_steps <- c(4, 8, 12, 16, 20, 30, 40, 80, 120, 160, 200, 400, 600, 800, 1000) * 1000


step_stats <- do.call(rbind, lapply(main_steps, function(ith_step) {
  sub_dat <- select(res_work, 1, which(all_steps == ith_step)) 
  colnames(sub_dat)[2L:3] <- c("male", "female")
  
  group_by(sub_dat, replicate) %>% 
    summarise(male = sum(male), female = sum(female)) %>% 
    cbind(step = ith_step, .)
})) %>% mutate(total = male + female, ratio = male/female) %>% 
  cbind(status = "all", .)

step_children <- do.call(rbind, lapply(main_steps, function(ith_step) {
  sub_dat <- res_work[which(all_steps %in% (ith_step - 9):ith_step)]
  sub_dat_male <- cbind(res_work["replicate"], sub_dat[grep("a1", colnames(sub_dat))])
  male_mean <- group_by(sub_dat_male, replicate) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-replicate) %>% 
    rowMeans()
  
  sub_dat_female <- cbind(res_work["replicate"], sub_dat[grep("a2", colnames(sub_dat))])
  female_mean <- group_by(sub_dat_female, replicate) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-replicate) %>% 
    rowMeans()
  
  data.frame(unique(res_work[1]),
             male = male_mean,
             female = female_mean) %>% 
    mutate(total = male + female, ratio = male/female) %>% 
    cbind(step = ith_step, .)
})) %>% 
  cbind(status = "B", .)

before_reproduction <- do.call(rbind, lapply(main_steps, function(ith_step) {
  sub_dat <- select(res_work, 1, which(all_steps == ith_step)) 
  colnames(sub_dat)[2L:3] <- c("male", "female")
  
  group_by(sub_dat, replicate) %>% 
    slice(1L:30) %>% 
    summarise(male = sum(male), female = sum(female)) %>% 
    cbind(step = ith_step, .)
})) %>% 
  mutate(total = male + female, ratio = male/female) %>% 
  cbind(status = "<R", .)
  
after_reproduction <- do.call(rbind, lapply(main_steps, function(ith_step) {
  sub_dat <- select(res_work, 1, which(all_steps == ith_step)) 
  colnames(sub_dat)[2L:3] <- c("male", "female")
  
  group_by(sub_dat, replicate) %>% 
    slice(31:53) %>% 
    summarise(male = sum(male), female = sum(female)) %>% 
    cbind(step = ith_step, .)
})) %>% 
  mutate(total = male + female, ratio = male/female) %>% 
  cbind(status = ">R", .)

all_replicates <- rbind(step_stats, step_children, before_reproduction, after_reproduction)

summary_stats <- all_replicates %>% 
  mutate(step = factor(step)) %>% 
  group_by(status, step) %>% 
  summarise_each(funs(mean), -replicate) 

all_replicates_chr <- mutate(all_replicates, 
                             n_y = male,
                             n_x = 2*female + male,
                             n_a = (male+female)*2)



# defect analysis --------------------------------

res_defect <- do.call(rbind, lapply(replicate_list, function(ith_replicate) {
  all_files <- list.files(paste0(ith_replicate, "/"))
  chr_files <- all_files[grepl(".txt", all_files, fixed = TRUE) & grepl("sc_chr", all_files)]
  conf_file <- all_files[!grepl(".", all_files, fixed = TRUE)]
  
  chr_pop <- readLines(paste0(ith_replicate, "/", conf_file))[1]
  
  raw_ranges <- strsplit(chr_pop, "[", fixed = TRUE)[[1]] %>%
    last %>% 
    strsplit("]", fixed = TRUE) %>% 
    unlist %>% 
    first %>% 
    strsplit(",", fixed = TRUE) %>% 
    unlist %>% 
    as.numeric
  
  ####################################            |
  #                                               |
  #               MNOŻNIK 64                    #####
  #                                              ###
  ###########################################     #
  chr_ranges <- c(raw_ranges, last(raw_ranges)) * 64
  
  chr_ind <- lapply(1L:length(chr_ranges), function(single_chr) {
    rep(single_chr, chr_ranges[single_chr])
  }) %>% 
    unlist %>% 
    factor
  
  birth_age <- readLines(paste0(ith_replicate, "/", conf_file))[7] %>% 
    strsplit("p.setBirthAge(", fixed = TRUE) %>% 
    unlist %>% 
    last %>% 
    strsplit(")", fixed = TRUE) %>% 
    unlist %>% 
    first %>% 
    as.numeric
  
  reprod_age <- readLines(paste0(ith_replicate, "/", conf_file))[8] %>% 
    strsplit("p.setReproductionAge(", fixed = TRUE) %>% 
    unlist %>% 
    last %>% 
    strsplit(")", fixed = TRUE) %>% 
    unlist %>% 
    first %>% 
    as.numeric
  
  chr_names <- 1L:length(chr_ranges)
  chr_names[length(chr_ranges)] <- "Y"
  chr_names[length(chr_ranges) - 1] <- "X"
  
  chr_dat <- do.call(rbind, lapply(chr_files, function(ith_file) {
                     
                     res <- read.table(paste0(ith_replicate, "/", ith_file)) %>%
                       unlist %>% 
                       split(chr_ind)
                     
                     total <- sapply(res, sum)
                     
                     birth <- sapply(res, function(single_chr) {
                       sum(single_chr[1L:(birth_age/128 * length(single_chr))])
                     })
                     
                     reprod <- sapply(res, function(single_chr) {
                       sum(single_chr[1L:(reprod_age/128 * length(single_chr))])
                     })
                     
                     youth <- reprod - birth
                     
                     adults <- sapply(res, function(single_chr) {
                       sum(single_chr[(reprod_age/128 * length(single_chr) + 1):length(single_chr)])
                     })
                     
                     step_name <- strsplit(ith_file, "sc_chr") %>% 
                       unlist %>% 
                       last %>% 
                       strsplit("c.txt", fixed = TRUE) %>% 
                       unlist %>% 
                       as.numeric
                     
                     lengths_chr <- lengths(res)
                     lengths_chr_df <- data.frame(chr = chr_names,
                                                  total = lengths_chr, 
                                                  birth = birth_age/128 * lengths_chr, 
                                                  reprod = reprod_age/128 * lengths_chr,
                                                  youth = reprod_age/128 * lengths_chr - birth_age/128 * lengths_chr, 
                                                  adults = lengths_chr - reprod_age/128 * lengths_chr) %>% 
                       melt(variable.name = "defect_status",
                            value.name = "n_chr",
                            id.var = "chr") 
                     
                     defect_chr_df <- data.frame(chr = chr_names,
                                                 total = total, 
                                                 birth = birth, 
                                                 reprod = reprod, 
                                                 youth = youth, 
                                                 adults = adults) %>% 
                       melt(variable.name = "defect_status",
                            value.name = "n_defect",
                            id.var = "chr") 
                     
                     data.frame(replicate = ith_replicate,
                                step = step_name,
                                inner_join(defect_chr_df, lengths_chr_df, 
                                           by = c("chr", "defect_status")))
                   }))
}))

joined_chr <- filter(all_replicates_chr, status == "all") %>% 
  select(step, replicate, n_x, n_y, n_a) %>% 
  inner_join(res_defect, by = c("step", "replicate")) 

norm_chr_raw <- rbind(filter(joined_chr, chr == "X") %>% 
        mutate(normalized = n_defect/(n_chr * n_x)),
      filter(joined_chr, chr == "Y") %>% 
        mutate(normalized = n_defect/(n_chr * n_y)),
      filter(joined_chr, !(chr %in% c("X", "Y"))) %>% 
        mutate(normalized = n_defect/(n_chr * n_a)))

norm_chr <- rbind(cbind(filter(norm_chr_raw, chr == "X"), 
            ch10 = filter(norm_chr_raw, chr == "10")[["normalized"]]) %>% 
        mutate(normalized = normalized/ch10,
               chr = "X/10"),
      cbind(filter(norm_chr_raw, chr == "Y"), 
            ch10 = filter(norm_chr_raw, chr == "10")[["normalized"]]) %>% 
        mutate(normalized = normalized/ch10,
               chr = "Y/10")) %>% 
  select(-ch10) %>% 
  rbind(norm_chr_raw, .) %>% 
  mutate(chr = suppressWarnings(factor(chr, levels = levels(chr)[order(as.numeric(levels(chr)))]))) %>% 
  arrange(replicate, step, chr, defect_status) 

summ_norm_chr <- group_by(norm_chr, step, chr, defect_status) %>% 
  summarise(normalized = mean(normalized)) 

# recombination evolution --------------------------------

recomb_res <- do.call(rbind, lapply(replicate_list, function(ith_replicate) {
  all_files <- list.files(paste0(ith_replicate, "/"))
  all_lines <- readLines(paste0(ith_replicate, "/", all_files[grepl("xyCnt", all_files)])) 
  res <- read.table(textConnection(all_lines[all_lines != ""]))
  colnames(res) <- c("num", "p", "XYrr")
  cbind(replicate = ith_replicate, res)
})) 

recomb_res_summ <- group_by(recomb_res, num) %>% 
  summarise(XYrr = mean(XYrr))


# save results -----------------------------------

dir_res_name <- paste0(dir_name, "_results_n", length(replicate_list), "/")
dir.create(dir_res_name)

WriteXLS(all_replicates_chr, ExcelFileName = paste0(dir_res_name, dir_name, "_all_replicates.xlsx"))
WriteXLS(res_work, ExcelFileName = paste0(dir_res_name, dir_name, "_raw.xlsx"))
WriteXLS(summary_stats, ExcelFileName = paste0(dir_res_name, dir_name, ".xlsx"))

WriteXLS(res_defect, ExcelFileName = paste0(dir_res_name, dir_name, "_defect.xlsx"))
WriteXLS(norm_chr, ExcelFileName = paste0(dir_res_name, dir_name, "_norm_defect.xlsx"))
WriteXLS(summ_norm_chr, ExcelFileName = paste0(dir_res_name, dir_name, "_summ_norm_defect.xlsx"))

WriteXLS(recomb_res, ExcelFileName = paste0(dir_res_name, dir_name, "_recomb.xlsx"))
WriteXLS(recomb_res_summ, ExcelFileName = paste0(dir_res_name, dir_name, "_summ_recomb.xlsx"))
