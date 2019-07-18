library(glue)
library(tidyverse)
library(chemhelper)
# library(ggstance)
library(ggbeeswarm)
library(plotly)
library(naniar)

data_dir <- ("/Users/scottericr/Documents/ia-batch-tools/testdir")
data_dir
folders <- list.files(data_dir)
folders

# sample_choice <- c("090117_BACE_A2", "090117_BACE_A6")
sample_choice <- folders

chosen_folders <- list.files(data_dir, full.names = TRUE)[list.files(data_dir) %in% sample_choice]
chosen_folders


reports <- list.files(chosen_folders, pattern = "*\\.csv")
reports

df <- as.data.frame(table(reports))
df
report_options <- glue("{df$reports} ({df$Freq})")
report_options

report_choice <- "All Cuts Tea Integration Report.csv"

full_paths <- paste0(chosen_folders, "/", report_choice) %>% set_names(sample_choice)

data_raw <- map_df(full_paths, read_IA, .id = "sample")

df <- data_raw %>% 
  janitor::clean_names() %>% 
  rename(rt = r_time_min, rt_exp = expect_min, rt_window_st = st_time_min,
         rt_window_end = end_time_min) %>% 
  #set zeroes to NAs
  mutate_if(is.double, ~ifelse(. == 0, NA, .)) %>% 
  #calculate deviations from expected RT
  mutate(rt_dev = rt - rt_exp,
         rt_dev_start = rt_window_st - rt_exp,
         rt_dev_end = rt_window_end - rt_exp) %>% 
  group_by(compound) %>% 
  #remove compounds that don't appear in any files
  filter(!all(is.na(rt))) %>%
  #calculate standard deviation within compound
  mutate(rt_sd = sd(rt, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # reorder compounds based on their standard deviation so most problematic ones show up in first page
  mutate(no = as.factor(no) %>% fct_reorder(rt_sd, .desc = TRUE)) %>% 
  mutate(compound_trunc = glue("({no}) {str_trunc(compound, 15)}")) %>% 
  select(-type) #i don't know what the type column is even

nperpage <- 5 #make input for this later?
#figure out pages for plots
lvls <-
  #get unique levels of compound number in order
  tibble(no = fct_unique(fct_drop(df$no))) %>%
  #add a column that puts them in groups of nperpage (currently 20)
  mutate(page = rep(1:ceiling(n()/nperpage), each = nperpage, length.out = n()))
#join that to the original data so there is a grouping variable called "page"
data_cleaned <- full_join(df, lvls) %>%
  arrange(desc(rt_sd)) %>% 
  mutate(rownum = row_number())



# View(data_cleaned)
page = 6
# diagnostic plot
j <- position_jitter(height = 0.2, width = 0)
p <- ggplot(data_cleaned %>%  
              #only plot one page at a time
              filter(page == 5),
            aes(x = rt_dev,
                y = fct_inorder(compound_trunc),
                color = q_val,
                text = glue("File: {sample}
                            Compound: {compound_trunc}
                            RT: {round(rt, 3)}
                            RT expected: {round(rt_exp, 3)}
                            Q Value: {round(q_val, 3)}"))) +
  geom_miss_point(alpha = 0.5, position = j) +
  geom_errorbarh(aes(xmin = rt_dev_start, xmax = rt_dev_end, height = 0, width = 0),
                 height = 0, width = 0,
                 alpha = 0.4, position = j) +
  scale_color_viridis_c(option = "C") +
  coord_cartesian(xlim = c(-0.5, 0.5)) +
  labs(x = "deviation from expected RT", y = "(No.) Compound",
       # title = "Compounds sorted by standard deviation of retention time",
       color = "Q")

ggplotly(p, tooltip = c("text")) %>% 
  layout(xaxis = list(fixedrange = TRUE)) #only allow zooming and panning along y-axis




#flipped plot
j <- position_jitter(height = 0, width = 0.2)
p <- ggplot(data_cleaned %>%  
              #only plot one page at a time
              filter(page == 5),
            aes(y = rt_dev,
                x = fct_inorder(compound_trunc),
                color = q_val,
                text = glue("File: {sample}
                            Compound: {compound_trunc}
                            RT: {round(rt, 3)}
                            RT expected: {round(rt_exp, 3)}
                            Q Value: {round(q_val, 3)}"))) +
  geom_miss_point(alpha = 0.5, position = j) +
  geom_linerange(aes(ymin = rt_dev_start, ymax = rt_dev_end),
                 alpha = 0.4, position = j) +
  scale_color_viridis_c(option = "C") +
  coord_flip(ylim = c(-0.5, 0.5)) +
  labs(x = "deviation from expected RT", y = "(No.) Compound",
       # title = "Compounds sorted by standard deviation of retention time",
       color = "Q")

ggplotly(p, tooltip = c("text")) %>% 
  layout(xaxis = list(fixedrange = TRUE)) #only allow zooming and panning along y-axis





qtable <- data_cleaned %>% 
  select(sample, no, compound, compound_trunc, q_val) %>% 
  group_by(compound) %>% 
  mutate(`Mean Q Value` = mean(q_val, na.rm = TRUE)) %>%
  filter(!is.na(q_val))

qplot <- qtable %>%
  ungroup() %>% 
  mutate(sample = fct_reorder(sample, q_val),
         compound_trunc = fct_reorder(compound_trunc, q_val)) %>% 
  ggplot(aes(x = sample, y = compound_trunc, fill = q_val)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(x = "low <-- sample mean Q-value --> high",
       y = "low <-- compound mean Q-value --> high") 






ggplotly(qplot) %>%  plotly::layout(xaxis = list(tickangle = -90))

ggplot() +
  geom_blank() + 
  annotate("segment", x = -1, xend = 1, y = 0, yend = 0, arrow = arrow(ends = "both")) +
  annotate("segment", x = 0, xend = 0, y = -1, yend = 1, arrow = arrow(ends = "both")) +
  annotate("text", x = 0.90, y = -0.08, label = "High Sample Q-Value") +
  annotate("text", x = -0.9, y = -0.08, label = "Low Sample Q-Value") +
  
  annotate("text", x = 0, y = 1.05, label = "High Compound Q-Value") +
  annotate("text", x = 0, y = -1.05, label = "Low Compound Q-Value") +
  labs(x = "Sample (arranged by mean Q-value across all compounds)",
       y = "Compound (arranged by mean Q-value across all samples)") +
  theme_void() + 
  theme(axis.title = element_text(size = 14),
        axis.title.y = element_text(angle = 90),
        # axis.line = element_line(),
        panel.background = element_rect(fill = "white", color = "black", size = 1))


df <- tibble(sample = rep(LETTERS[1:5], length.out = 25),
             no = rep(1:5, each = 5),
             compound = paste("chemical", no),
             RT = rnorm(25),
             ExpectedRT = rnorm(25),
             RTsd = rnorm(25),
             q_val = runif(25, 85, 100))


df %>% arrange(sample) %>% #View()
  select(sample, no, compound, q_val) %>%
  group_by(compound) %>% 
  mutate(mean_q = mean(q_val),
         sd_q = sd(q_val)) %>% 
  
  spread(key = sample, value = q_val) %>%
  arrange(mean_q) %>% View()


# Isomer table
tol = 0.005
data_cleaned %>% 
  arrange(sample, rt) %>% #important for lag() and lead()
  mutate(rt_diff = rt - lag(rt), rt_diff2 = rt - lead(rt)) %>% 
  mutate(compound2 = case_when(rt_diff < tol ~ lag(compound),
                               rt_diff2 > -tol ~ lead(compound)),
         no2 = case_when(rt_diff < tol ~ lag(no),
                         rt_diff2 > -tol ~ lead(no)),
         main_ion2 = case_when(rt_diff < tol ~ lag(main_ion_m_z),
                               rt_diff2 > -tol ~ lead(main_ion_m_z)),
         rt2 = case_when(rt_diff < tol ~ lag(rt),
                         rt_diff2 > -tol ~ lead(rt))) %>% 
  dplyr::filter(rt_diff < tol | rt_diff2 > -tol) %>%
  select(sample, no, compound,"main ion (m/z)" = main_ion_m_z, "expected RT" = rt_exp, RT = rt, 
         "no. 2" = no2, "compound 2" = compound2, "main ion 2" = main_ion2, "RT 2" = rt2) %>%
  arrange(RT) %>% DT::datatable(
         
         # server = FALSE, #not good for large data
         extensions = c("Buttons"), filter = "top",
         options = list(
           dom = "lBfrtip",
           buttons = c("copy", "csv", "excel")))



