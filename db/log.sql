--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: log; Type: TABLE; Schema: public; Owner: irc; Tablespace: 
--

CREATE TABLE log (
    id bigint NOT NULL,
    nick text NOT NULL,
    utctime timestamp with time zone,
    message text
);


ALTER TABLE log OWNER TO irc;

--
-- Name: log_id_seq; Type: SEQUENCE; Schema: public; Owner: irc
--

CREATE SEQUENCE log_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE log_id_seq OWNER TO irc;

--
-- Name: log_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: irc
--

ALTER SEQUENCE log_id_seq OWNED BY log.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: irc
--

ALTER TABLE ONLY log ALTER COLUMN id SET DEFAULT nextval('log_id_seq'::regclass);


--
-- Data for Name: log; Type: TABLE DATA; Schema: public; Owner: irc
--

COPY log (id, nick, utctime, message) FROM stdin;
\.


--
-- Name: log_id_seq; Type: SEQUENCE SET; Schema: public; Owner: irc
--

SELECT pg_catalog.setval('log_id_seq', 66, true);


--
-- Name: log_pkey; Type: CONSTRAINT; Schema: public; Owner: irc; Tablespace: 
--

ALTER TABLE ONLY log
    ADD CONSTRAINT log_pkey PRIMARY KEY (id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

