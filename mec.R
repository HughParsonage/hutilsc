libraries();
de_in()
Edges <- fread("~/dhhs/dhhs-names/Edges.tsv", key = "from,to")
ego_net(1L, order = 4L, Edges = Edges)

