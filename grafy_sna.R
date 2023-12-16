library(dplyr)
library(igraph)
library(ggraph)
library(RSQLite)
library(tidygraph)
library(extrafont)
library(ggforce)
library(graphlayouts)

set.seed(1234)

con <- dbConnect(SQLite(), "data/2023-11-09_Vzkum PJTN.sqlite3")

data <- tbl(con, "highlights") %>% 
    left_join(., tbl(con, "highlight_tags"), by = c("id"="highlight_id")) %>% 
    left_join(., tbl(con, "tags"), by = c("tag_id"="id")) %>% 
    collect() %>% 
    filter(!is.na(tag_id)) 

dbDisconnect(con)

data_clean <- data %>% 
    mutate(name_short = stringr::str_extract(path, "([A-Ža-ž, ]+$)", group = 1)) %>% 
    mutate(name_short = case_when(
        grepl("alkohol", name_short) ~ "Alkohol a jiné drogy", 
        grepl("Mocensky", path) & name_short != "velký věkový rozdíl" ~ "mocensky asymetrické vztahy",
        TRUE ~ name_short)) %>% 
    select(name_short, document_id) %>% 
    unique()


create_cooccurrence_matrix <- function(data){
    docs <- factor(data$document_id)
    tags <- factor(data$name_short)
    
    s <- Matrix::sparseMatrix(
        as.numeric(docs), 
        as.numeric(tags),
        dimnames = list(
            as.character(levels(docs)), 
            as.character(levels(tags))),
        x = 1)
    
    # calculating co-occurrences
    Matrix::crossprod(s)
}

matrix <- create_cooccurrence_matrix(data_clean)

tags_count <- data_clean %>% 
    count(name_short) 

graph <- graph.adjacency(matrix, 
                         mode = 'undirected', 
                         weighted = TRUE, 
                         diag = FALSE)

tbl_g <- as_tbl_graph(graph)

# font_import()

BLUE <- "#1017b6"
ORANGE <- "#ff5400"

g1 <- tbl_g %>% 
    activate(nodes) %>% 
    left_join(., tags_count, by = c("name"="name_short")) %>% 
    filter(n > 25) %>% 
    activate(edges) %>% 
    filter(weight > 15) %>% 
    activate(nodes) %>% 
    mutate(group =  group_louvain(weights = weight)) %>% 
    filter(degree(.) > 0) 

ggraph(g1, layout = "auto") + 
    geom_edge_link0(aes(edge_width = weight), alpha = 0.1) + 
    geom_node_point(aes(filter = degree(g1) > 0, size = n), colour = ORANGE) +
    geom_node_text(aes(label = name), repel = TRUE,
                   colour = BLUE, max.overlaps = 20, size = 4, 
                   fontface = "bold") +
    scale_colour_viridis_d(end = 0.8) + 
    theme_graph(base_family = "Poppins") +
    guides(colour = "none") + 
    labs(
        title = "Nejčastěji zmiňovaná témata a vazby mezi nimi",
        size = "Počet výskytu tématu",
        edge_width = "Spoluvýskyt tématu",
        caption = "Data: Proč jsme to nenahlásily*i, zobrazeny pouze body s více než 25 výskyty a vazby s více než 15 výskyty."
    )

ggsave("figs/graf_celkovy.png", width = 12, height = 7)

bb <- layout_with_stress(g1)

g1_first <- g1 %>% 
    activate(nodes) %>% 
    mutate(group_hull = if_else(group == 1, as.character(group), NA_character_), 
           label_hull = if_else(group == 1, "Kódy často zmiňované v případech sexuálního násilí na dětech", NA_character_), 
           description_hull = if_else(group == 1, "- popisek, ... ", NA_character_))

ggraph(g1_first,
       layout = "manual",
       x = bb[, 1],
       y = bb[, 2]) +
    geom_edge_link0(aes(edge_width = weight), alpha = 0.1) + 
    geom_node_point(aes(size = n, colour = factor(group))) +
    geom_mark_hull(
        aes(x, y, group = group_hull, 
            fill = group_hull, 
            filter = !is.na(group_hull)
            # label = label_hull,
            # description = description_hull
            ),
        concavity = 3,
        expand = unit(2, "mm"),
        alpha = 0.1, 
        show.legend = FALSE, 
        na.rm = TRUE, 
        size = 0
    ) +
    geom_node_text(aes(label = name, filter = !is.na(group_hull)), 
                   repel = TRUE, fontface = "bold",
                   colour = BLUE, max.overlaps = 20, size = 4) + 
    scale_colour_manual(values = c("1"=ORANGE, "2"="gray50", "3"="gray50", 
                                   "4"="gray50")) +
    scale_fill_manual(values = ORANGE) + 
    # scale_color_brewer(palette = "Set1") +
    # scale_fill_brewer(palette = "Set1") +
    theme_graph(base_family = "Poppins") +
    guides(colour = "none", fill = "none",
           size = "none", edge_width = "none") + 
    labs(
        title = "Nejčastěji zmiňovaná témata a vazby mezi nimi",
        subtitle = "Komponenta 1",
        # size = "Počet výskytu tématu",
        # edge_width = "Spoluvýskyt tématu",
        caption = "Data: Proč jsme to nenahlásily*i, zobrazeny pouze body s více než 25 výskyty a vazby s více než 15 výskyty."
    )

ggsave("figs/graf_komponenta1.png", width = 10, height = 7)

g1_snd <- g1 %>% 
    activate(nodes) %>% 
    mutate(group_hull = if_else(group == 2, as.character(group), NA_character_), 
           label_hull = if_else(group == 2, "Label", NA_character_), 
           description_hull = if_else(group == 2, "- popisek", NA_character_))

ggraph(g1_snd,
       layout = "manual",
       x = bb[, 1],
       y = bb[, 2]) +
    geom_edge_link0(aes(edge_width = weight), alpha = 0.1) + 
    geom_node_point(aes(size = n, colour = factor(group))) +
    geom_mark_hull(
        aes(x, y, group = group_hull, 
            fill = group_hull, 
            filter = !is.na(group_hull) 
            # label = label_hull, 
            # description = description_hull
            ),
        concavity = 2,
        expand = unit(2, "mm"),
        alpha = 0.1, 
        show.legend = FALSE, 
        na.rm = TRUE, 
        size = 0
    ) +
    geom_node_text(aes(label = name, filter = !is.na(group_hull)), 
                   repel = TRUE, fontface = "bold",
                   colour = BLUE, max.overlaps = 20, size = 4) + 
    scale_colour_manual(values = c("2"=ORANGE, "1"="gray50", "3"="gray50", 
                                   "4"="gray50")) +
    scale_fill_manual(values = ORANGE) + 
    # scale_color_brewer(palette = "Set1") +
    # scale_fill_brewer(palette = "Set1") +
    theme_graph(base_family = "Poppins") +
    guides(colour = "none", fill = "none", 
           size = "none", edge_width = "none") + 
    labs(
        title = "Nejčastěji zmiňovaná témata a vazby mezi nimi",
        subtitle = "Komponenta 2",
        # size = "Počet výskytu tématu",
        # edge_width = "Spoluvýskyt tématu",
        caption = "Data: Proč jsme to nenahlásily*i, zobrazeny pouze body s více než 25 výskyty a vazby s více než 15 výskyty."
    )

ggsave("figs/graf_komponenta2.png", width = 10, height = 7)

g1_thd <- g1 %>% 
    activate(nodes) %>% 
    mutate(group_hull = if_else(group == 3, as.character(group), NA_character_), 
           label_hull = if_else(group == 3, "Label", NA_character_), 
           description_hull = if_else(group == 3, "- popisek", NA_character_))

ggraph(g1_thd,
       layout = "manual",
       x = bb[, 1],
       y = bb[, 2]) +
    geom_edge_link0(aes(edge_width = weight), alpha = 0.1) + 
    geom_node_point(aes(size = n, colour = factor(group))) +
    geom_mark_hull(
        aes(x, y, group = group_hull, 
            fill = group_hull, 
            filter = !is.na(group_hull)
            # label = label_hull, 
            # description = description_hull
            ),
        concavity = 2,
        expand = unit(2, "mm"),
        alpha = 0.1, 
        show.legend = FALSE, 
        na.rm = TRUE, 
        size = 0
    ) +
    geom_node_text(aes(label = name, filter = !is.na(group_hull)), 
                   repel = TRUE, fontface = "bold",
                   colour = BLUE, max.overlaps = 20, size = 4) + 
    scale_colour_manual(values = c("3"=ORANGE, "1"="gray50", "2"="gray50", 
                                   "4"="gray50")) +
    scale_fill_manual(values = ORANGE) + 
    # scale_color_brewer(palette = "Set1") +
    # scale_fill_brewer(palette = "Set1") +
    theme_graph(base_family = "Poppins") +
    guides(colour = "none", fill = "none", 
           size = "none", edge_width = "none") + 
    labs(
        title = "Nejčastěji zmiňovaná témata a vazby mezi nimi",
        subtitle = "Komponenta 3",
        # size = "Počet výskytu tématu",
        # edge_width = "Spoluvýskyt tématu",
        caption = "Data: Proč jsme to nenahlásily*i, zobrazeny pouze body s více než 25 výskyty a vazby s více než 15 výskyty."
    )

ggsave("figs/graf_komponenta3.png", width = 10, height = 7)
