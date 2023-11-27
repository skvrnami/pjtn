library(dplyr)
library(ggplot2)
library(ggsankey)

con <- dbConnect(SQLite(), "data/2023-11-09_Vzkum PJTN.sqlite3")

data <- tbl(con, "highlights") %>% 
    left_join(., tbl(con, "highlight_tags"), by = c("id"="highlight_id")) %>% 
    left_join(., tbl(con, "tags"), by = c("tag_id"="id")) %>% 
    collect() %>% 
    filter(!is.na(tag_id)) 

# Charakteristika
tmp2 <- data %>% 
    filter(grepl("Charakteristika", path)) %>% 
    mutate(path_original = path) %>% 
    tidyr::separate(., col = path, sep = "\\.", 
                    into = c("lvl1", "lvl2", "lvl3", "lvl4")) %>% 
    arrange(path_original) %>% 
    group_by(lvl1) %>% 
    mutate(
        lvl1_n = n(),
        lvl1 = glue::glue("{lvl1} ({lvl1_n})")
    ) %>% 
    ungroup %>% 
    group_by(lvl2) %>% 
    mutate(
        lvl2_n = n(),
        lvl2 = glue::glue("{lvl2} ({lvl2_n})")
    ) %>% 
    ungroup %>% 
    group_by(lvl3) %>% 
    mutate(
        lvl3_n = n(),
        lvl3 = glue::glue("{lvl3} ({lvl3_n})")
    ) %>% 
    ungroup %>% 
    group_by(lvl4) %>% 
    mutate(
        lvl4_n = n(),
        lvl4 = glue::glue("{lvl4} ({lvl4_n})")
    ) %>% 
    ungroup %>% 
    mutate(
        across(matches("lvl[1-4]{1}"), as.character), 
        across(matches("lvl[1-4]{1}"), ~if_else(grepl("^NA", .x), NA_character_, .x))
    ) # %>% 
    # mutate(
    #     rank_lvl1 = rank(lvl1_n), 
    #     rank_lvl2 = rank(lvl2_n), 
    #     rank_lvl3 = rank(lvl3_n), 
    #     rank_lvl4 = rank(lvl4_n)
    # ) %>% 
    # arrange(rank_lvl1, rank_lvl2, rank_lvl3, rank_lvl4)

node_levels2 <- unique(c(tmp2$lvl1, tmp2$lvl2, tmp2$lvl3, tmp2$lvl4))
# writeLines(node_levels2, "tmp/levels2.txt")
node_levels2_edited <- readLines("tmp/levels2.txt")

tmp_long2 <- make_long(tmp2, lvl1, lvl2, lvl3, lvl4) %>% 
    filter(!is.na(node)) %>% 
    mutate(node = factor(node, levels = node_levels2_edited), 
           next_node = factor(next_node, levels = node_levels2_edited))

ggplot(tmp_long2, aes(x = x, 
                     next_x = next_x, 
                     node = node, 
                     # node = factor(node, levels = node_levels2),
                     label = node, 
                     next_node = next_node)) +
    geom_sankey(width = 0.25, node.fill = BLUE, node.color = BLUE, 
                flow.fill = BLUE, flow.colour = BLUE) + 
    geom_sankey_label(colour = BLUE, fill = "white") +
    guides(fill = "none") +
    theme_sankey(base_family = "Poppins") + 
    labs(title = "Proč jsme to nenahlásily*i: charakteristika sexualizovaného násilí", 
         subtitle = "Témata zmiňované ve výpovědích a jejich četnost") + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank())

ggsave("figs/sankey_charakteristika.png", width = 10, height = 7)

tmp3 <- data %>% 
    filter(grepl("Přeživší", path) | grepl("Sebeobviňování", path)) %>% 
    mutate(path_original = path) %>% 
    tidyr::separate(., col = path, sep = "\\.", 
                    into = c("lvl1", "lvl2", "lvl3", "lvl4")) %>% 
    arrange(path_original) %>% 
    group_by(lvl1) %>% 
    mutate(
        n = n(),
        lvl1 = glue::glue("{lvl1} ({n})")
    ) %>% 
    ungroup %>% 
    group_by(lvl2) %>% 
    mutate(
        n = n(),
        lvl2 = glue::glue("{lvl2} ({n})")
    ) %>% 
    ungroup %>% 
    select(-n) %>% 
    mutate(
        across(matches("lvl[1-4]{1}"), ~if_else(grepl("^NA", .x), NA_character_, .x))
    )

node_levels3 <- unique(c(tmp3$lvl1, tmp3$lvl2, tmp3$lvl3, tmp3$lvl4))
# writeLines(node_levels3, "tmp/levels3.txt")
node_levels3_edited <- readLines("tmp/levels3.txt")

tmp_long3 <- make_long(tmp3, lvl1, lvl2, lvl3, lvl4) %>% 
    filter(!is.na(node)) %>% 
    mutate(node = factor(node, levels = node_levels3_edited), 
           next_node = factor(next_node, levels = node_levels3_edited))

ggplot(tmp_long3, aes(x = x, 
                      next_x = next_x, 
                      node = node,
                      label = node, 
                      next_node = next_node)) +
    geom_sankey(width = 0.25, node.fill = BLUE, node.color = BLUE, 
                flow.fill = BLUE, flow.color = BLUE) + 
    geom_sankey_label(colour = BLUE, fill = "white") +
    guides(fill = "none") +
    theme_sankey(base_family = "Poppins") + 
    labs(title = "Proč jsme to nenahlásily*i: reakce přeživší*ho", 
         subtitle = "Témata zmiňované ve výpovědích a jejich četnost") + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank())

ggsave("figs/sankey_prezivsi.png", width = 10, height = 7)

tmp4 <- data %>% 
    filter(!grepl("Charakteristika", path) & !grepl("Přeživší", path) & 
               !grepl("Sebeobviňování", path)) %>% 
    mutate(path_original = path) %>% 
    tidyr::separate(., col = path, sep = "\\.", 
                    into = c("lvl1", "lvl2", "lvl3", "lvl4")) %>% 
    arrange(path_original) %>% 
    group_by(lvl1) %>% 
    mutate(
        n = n(),
        lvl1 = glue::glue("{lvl1} ({n})")
    ) %>% 
    ungroup %>% 
    group_by(lvl2) %>% 
    mutate(
        n = n(),
        lvl2 = glue::glue("{lvl2} ({n})")
    ) %>% 
    ungroup %>% 
    select(-n) %>% 
    mutate(
        across(matches("lvl[1-4]{1}"), ~if_else(grepl("^NA", .x), NA_character_, .x))
    )

node_levels4 <- unique(c(tmp4$lvl1, tmp4$lvl2, tmp4$lvl3, tmp4$lvl4))
# writeLines(node_levels4, "tmp/levels4.txt")
node_levels4_edited <- readLines("tmp/levels4.txt")

tmp_long4 <- make_long(tmp4, lvl1, lvl2, lvl3, lvl4) %>% 
    filter(!is.na(node)) %>% 
    mutate(node = factor(node, levels = node_levels4_edited), 
           next_node = factor(next_node, levels = node_levels4_edited))

ggplot(tmp_long4, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      label = node, 
                      next_node = next_node)) +
    geom_sankey(width = 0.25, node.fill = BLUE, node.color = BLUE, 
                flow.fill = BLUE, flow.color = BLUE) + 
    geom_sankey_label(colour = BLUE, fill = "white") +
    guides(fill = "none") +
    theme_sankey(base_family = "Poppins") + 
    labs(title = "Proč jsme to nenahlásily*i: charakteristika/reakce pachatele a okolí", 
         subtitle = "Témata zmiňované ve výpovědích a jejich četnost") + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          text = element_text(family = "Poppins"))

ggsave("figs/sankey_pachatel.png", width = 10, height = 7)
