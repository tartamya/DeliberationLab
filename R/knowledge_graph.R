# =============================================================================
# knowledge_graph.R -- claim/evidence graph (igraph + visNetwork)
# -----------------------------------------------------------------------------
# The graph is two plain data.frames (nodes, edges) held in reactiveValues --
# easiest thing to append to, render, and export. The moderator produces typed
# nodes/edges each round; this module accumulates, renders, and exports them,
# and derives the Idea Evolution table.
# =============================================================================

empty_kg <- function() {
  list(
    nodes = data.frame(id = character(), label = character(), type = character(),
                       round = integer(), stringsAsFactors = FALSE),
    edges = data.frame(from = character(), to = character(), relation = character(),
                       round = integer(), stringsAsFactors = FALSE)
  )
}

# Append one round's nodes/edges. IDs are prefixed with the round number
# because the moderator is stateless across rounds and reuses generic IDs
# ("claim1") every round -- without scoping, dedup would collapse them and the
# graph would stall after round 1.
kg_add_round <- function(kg, nodes_list, edges_list, round_number) {
  id_prefix <- function(raw) paste0("r", round_number, "_", raw)
  if (length(nodes_list) > 0) {
    new_nodes <- do.call(rbind, lapply(nodes_list, function(n) {
      raw_id <- as.character(n$id %||% digest::digest(n$label, algo = "crc32"))
      data.frame(id = id_prefix(raw_id), label = as.character(n$label %||% "unlabeled"),
                 type = as.character(n$type %||% "Idea"), round = round_number,
                 stringsAsFactors = FALSE)
    }))
    kg$nodes <- rbind(kg$nodes, new_nodes)
    kg$nodes <- kg$nodes[!duplicated(kg$nodes$id), ]
  }
  if (length(edges_list) > 0) {
    valid_ids <- kg$nodes$id
    new_edges <- do.call(rbind, lapply(edges_list, function(e) {
      f <- id_prefix(as.character(e$from)); t <- id_prefix(as.character(e$to))
      if (!(f %in% valid_ids) || !(t %in% valid_ids)) return(NULL)
      data.frame(from = f, to = t, relation = as.character(e$relation %||% "Relates"),
                 round = round_number, stringsAsFactors = FALSE)
    }))
    if (!is.null(new_edges) && nrow(new_edges) > 0) kg$edges <- rbind(kg$edges, new_edges)
  }
  kg
}

kg_node_color <- function(type) {
  switch(type,
         "Idea" = "#4C9AFF", "Claim" = "#57D9A3", "Evidence" = "#FFAB00",
         "Question" = "#998DD9", "Assumption" = "#B3BAC5", "Counterargument" = "#FF5630",
         "#97A0AF")
}

render_kg_visnetwork <- function(kg) {
  if (nrow(kg$nodes) == 0) {
    return(visNetwork::visNetwork(data.frame(id = 1, label = "No graph data yet"), data.frame()))
  }
  vis_nodes <- data.frame(
    id = kg$nodes$id, label = kg$nodes$label,
    title = paste0("<b>", kg$nodes$type, "</b><br>", kg$nodes$label, "<br>round ", kg$nodes$round),
    color = vapply(kg$nodes$type, kg_node_color, character(1)), shape = "dot", size = 18,
    stringsAsFactors = FALSE)
  vis_edges <- if (nrow(kg$edges) == 0) data.frame(from = character(), to = character()) else data.frame(
    from = kg$edges$from, to = kg$edges$to, label = kg$edges$relation, arrows = "to",
    color = ifelse(kg$edges$relation %in% c("Contradicts", "Rejects"), "#FF5630", "#97A0AF"),
    stringsAsFactors = FALSE)
  visNetwork::visNetwork(vis_nodes, vis_edges) |>
    visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
    visNetwork::visLayout(randomSeed = 42) |>
    visNetwork::visPhysics(stabilization = TRUE)
}

kg_to_igraph <- function(kg) {
  if (nrow(kg$nodes) == 0) return(igraph::make_empty_graph())
  igraph::graph_from_data_frame(
    d = if (nrow(kg$edges) > 0) kg$edges[, c("from", "to", "relation")] else
      data.frame(from = character(), to = character(), relation = character()),
    vertices = kg$nodes, directed = TRUE)
}

export_graphml <- function(kg, path) {
  igraph::write_graph(kg_to_igraph(kg), path, format = "graphml")
  path
}

# Idea Evolution: classify each Idea/Claim as New / Alive / Merged / Dead by
# recency and connectivity heuristics.
idea_evolution_table <- function(kg, current_round, stale_after = 4) {
  ideas <- kg$nodes[kg$nodes$type %in% c("Idea", "Claim"), ]
  if (nrow(ideas) == 0) {
    return(data.frame(id = character(), label = character(), born_round = integer(),
                      status = character(), influence = integer()))
  }
  do.call(rbind, lapply(seq_len(nrow(ideas)), function(i) {
    id <- ideas$id[i]
    touching <- kg$edges[kg$edges$from == id | kg$edges$to == id, ]
    last_touch <- if (nrow(touching) > 0) max(touching$round) else ideas$round[i]
    status <- if (any(kg$edges$to == id & kg$edges$relation %in% c("Extends", "Refines"))) "Merged/Refined"
    else if ((current_round - last_touch) > stale_after) "Dead"
    else if (ideas$round[i] == current_round) "New"
    else "Alive"
    data.frame(id = id, label = ideas$label[i], born_round = ideas$round[i],
               status = status, influence = nrow(touching), stringsAsFactors = FALSE)
  }))
}

# Compact one-line KG summary for injection into agent prompts.
kg_summary_text <- function(kg, n = 8) {
  if (nrow(kg$nodes) == 0) return("(empty)")
  paste(utils::tail(kg$nodes$label, n), collapse = "; ")
}
