-- To execute this file from SQL REPL:
-- \i sql/schema.sql

-- for hashing passwords
CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS users
( id         TEXT                     NOT NULL
, email      TEXT                     NOT NULL
, name       TEXT                     NOT NULL
, pwd_hash   TEXT                     NOT NULL
, created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

ALTER TABLE ONLY users
  ADD CONSTRAINT pk_users PRIMARY KEY (id);

CREATE TABLE IF NOT EXISTS accounts
( no         TEXT                     NOT NULL
, name       TEXT                     NOT NULL
, open_date  TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
, close_date TIMESTAMP WITH TIME ZONE 
, user_id    TEXT                     NOT NULL
, PRIMARY KEY(no)
, CONSTRAINT fk_user
    FOREIGN KEY(user_id) 
	    REFERENCES users(id)
)