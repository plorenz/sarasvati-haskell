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

create table wf_node_arc
(
  a_node_id int          NOT NULL REFERENCES wf_node,
  z_node_id int          NOT NULL REFERENCES wf_node,
  name      varchar(255) NOT NULL
)

create table wf_token
(
  id           serial NOT NULL PRIMARY KEY,
  prev_node_id int    NOT NULL REFERENCES wf_node,
  curr_node_id int    NOT NULL REFERENCES wf_node,
  next_node_id int    NOT NULL REFERENCES wf_node
)