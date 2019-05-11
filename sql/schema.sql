-- To execute this file from SQL REPL:
-- \i sql/schema.sql

CREATE TABLE IF NOT EXISTS users
( id         TEXT                     NOT NULL
, email      TEXT                     NOT NULL
, name       TEXT                     NOT NULL
, pwd_hash   TEXT                     NOT NULL
, created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

ALTER TABLE ONLY users
  ADD CONSTRAINT pk_users PRIMARY KEY (id);
