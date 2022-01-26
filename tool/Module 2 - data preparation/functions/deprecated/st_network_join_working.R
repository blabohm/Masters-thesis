st_network_join_working <- function(x, y) {
  st_network_join(activate(x, "nodes"), 
                  activate(y, "nodes"), 
                  suffix = c(".x", ".y")) %>% 
    mutate(identifier = coalesce(identifier.x, identifier.y),
           code_2018 = coalesce(code_2018.x, code_2018.y),
           population = coalesce(population.x, population.y),
           area = coalesce(area.x, area.y),
           detour_index = coalesce(detour_index.x, detour_index.y),
           di_count = coalesce(di_count.x, di_count.y),
           node_id = coalesce(node_id.x, node_id.y),
           .tidygraph_node_index = coalesce(.tidygraph_node_index.x, 
                                            .tidygraph_node_index.y)) %>% 
    select(identifier, code_2018, population, area, node_id, detour_index,
           di_count, .tidygraph_node_index)}
