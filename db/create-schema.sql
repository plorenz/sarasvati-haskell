create table wf_graph
(
  id      serial       NOT NULL PRIMARY KEY,
  name    varchar(255) NOT NULL,
  version int          NOT NULL
)

create table wf_node
(
  id              serial       NOT NULL PRIMARY KEY,
  graph_id        int          NOT NULL REFERENCES wf_graph,
  name            varchar(255) NOT NULL,
  require_all     boolean      NOT NULL,
  source_graph_id int          NOT NULL REFERENCES wf_graph,
  type            varchar(255) NOT NULL
)

create table wf_arc
(
  id        serial       NOT NULL PRIMARY KEY,
  a_node_id int          NOT NULL REFERENCES wf_node,
  z_node_id int          NOT NULL REFERENCES wf_node,
  name      varchar(255) NOT NULL
)

create table wf_node_token
(
  id           serial NOT NULL PRIMARY KEY,
  node_id      int    NOT NULL REFERENCES wf_node
)

create table wf_arc_token
(
  id     serial NOT NULL PRIMARY KEY,
  arc_id int    NOT NULL REFERENCES wf_arc
)