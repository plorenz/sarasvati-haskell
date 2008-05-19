drop table if exists wf_node_token;
drop table if exists wf_arc_token;
drop table if exists wf_arc;
drop table if exists wf_node_task;
drop table if exists wf_node;
drop table if exists wf_node_source;
drop table if exists wf_graph;

create table wf_graph
(
  id      serial       NOT NULL PRIMARY KEY,
  name    varchar(255) NOT NULL,
  version int          NOT NULL
);

create table wf_node_source
(
  id       serial       NOT NULL PRIMARY KEY,
  graph_id int          NOT NULL REFERENCES wf_graph,
  instance varchar(255) NOT NULL,
  depth    int          NOT NULL
);

create table wf_node
(
  id              serial       NOT NULL PRIMARY KEY,
  graph_id        int          NOT NULL REFERENCES wf_graph,
  source_id       int          NOT NULL REFERENCES wf_node_source,
  name            varchar(255) NOT NULL,
  is_join         boolean      NOT NULL,
  type            varchar(255) NOT NULL
);

create table wf_arc
(
  id        serial       NOT NULL PRIMARY KEY,
  a_node_id int          NOT NULL REFERENCES wf_node,
  z_node_id int          NOT NULL REFERENCES wf_node,
  name      varchar(255) NOT NULL
);

create table wf_node_token
(
  id           serial NOT NULL PRIMARY KEY,
  node_id      int    NOT NULL REFERENCES wf_node
);

create table wf_arc_token
(
  id     serial NOT NULL PRIMARY KEY,
  arc_id int    NOT NULL REFERENCES wf_arc
);

create table wf_node_task
(
  id int NOT NULL PRIMARY KEY REFERENCES wf_node,
  name varchar(255) NOT NULL,
  description text NOT NULL
)