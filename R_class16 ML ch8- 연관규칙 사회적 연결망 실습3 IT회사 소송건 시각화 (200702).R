###########연습해 볼 데이터##################
links = '[
{"source": "Microsoft", "target": "Amazon", "type": "licensing"},
{"source": "Microsoft", "target": "HTC", "type": "licensing"},
{"source": "Samsung", "target": "Apple", "type": "suit"},
{"source": "Motorola", "target": "Apple", "type": "suit"},
{"source": "Nokia", "target": "Apple", "type": "resolved"},
{"source": "HTC", "target": "Apple", "type": "suit"},
{"source": "Kodak", "target": "Apple", "type": "suit"},
{"source": "Microsoft", "target": "Barnes & Noble", "type": "suit"},
{"source": "Microsoft", "target": "Foxconn", "type": "suit"},
{"source": "Oracle", "target": "Google", "type": "suit"},
{"source": "Apple", "target": "HTC", "type": "suit"},
{"source": "Microsoft", "target": "Inventec", "type": "suit"},
{"source": "Samsung", "target": "Kodak", "type": "resolved"},
{"source": "LG", "target": "Kodak", "type": "resolved"},
{"source": "RIM", "target": "Kodak", "type": "suit"},
{"source": "Sony", "target": "LG", "type": "suit"},
{"source": "Kodak", "target": "LG", "type": "resolved"},
{"source": "Apple", "target": "Nokia", "type": "resolved"},
{"source": "Qualcomm", "target": "Nokia", "type": "resolved"},
{"source": "Apple", "target": "Motorola", "type": "suit"},
{"source": "Microsoft", "target": "Motorola", "type": "suit"},
{"source": "Motorola", "target": "Microsoft", "type": "suit"},
{"source": "Huawei", "target": "ZTE", "type": "suit"},
{"source": "Ericsson", "target": "ZTE", "type": "suit"},
{"source": "Kodak", "target": "Samsung", "type": "resolved"},
{"source": "Apple", "target": "Samsung", "type": "suit"},
{"source": "Kodak", "target": "RIM", "type": "suit"},
{"source": "Nokia", "target": "Qualcomm", "type": "suit"}
]'
link_df = jsonlite::fromJSON(links)
# node의 index 숫자는 0부터 시작해야 한다
# dplyr::row_number()가 1부터 숫자를 매기기 때문에 거기서 1씩을 빼도록 한다

node_df = data.frame(node = unique(c(link_df$source, link_df$target))) %>% mutate(idx = row_number()-1)
# node_df에서 index값을 가져와서 source와 target에 해당하는 index 값을 저장한다

link_df = link_df %>% 
  left_join(node_df %>% rename(source_idx = idx), by=c('source' = 'node')) %>% 
  left_join(node_df %>% rename(target_idx = idx), by=c('target' = 'node'))

# 데이터 확인
node_df
link_df
D3_network_LM<-forceNetwork(Links = link_df, 
                            Nodes = node_df, 
                            Source = 'source_idx', 
                            Target = 'target_idx', 
                            NodeID = 'node', 
                            Group = 'idx',
                            opacityNoHover = TRUE, 
                            zoom = TRUE, 
                            bounded = TRUE,
                            fontSize = 15,linkDistance = 75,
                            opacity = 0.9)
D3_network_LM 