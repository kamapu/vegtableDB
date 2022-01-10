-- Rename old table samples
ALTER TABLE swea_dataveg.samples RENAME TO samples_old;

-- Reset sequency
ALTER SEQUENCE swea_dataveg.samples_record_id_seq RESTART WITH 1;

-- Create table anew
CREATE TABLE swea_dataveg.samples
(
    record_id SERIAL PRIMARY KEY,
	"ReleveID" integer NOT NULL
		REFERENCES swea_dataveg.header ("ReleveID")
		ON UPDATE CASCADE ON DELETE NO ACTION,
    "TaxonUsageID" integer NOT NULL
    	REFERENCES tax_commons."taxonNames" ("TaxonUsageID")
        ON UPDATE CASCADE ON DELETE NO ACTION,
    veg_layer integer
		REFERENCES swea_dataveg.veg_layer (veg_layer)
        ON UPDATE CASCADE ON DELETE NO ACTION,
    misspelled_name text,
    cover_percentage numeric(4,1),
    sociability integer,
    br_bl text
    	REFERENCES commons.br_bl (symbol)
        ON UPDATE CASCADE ON DELETE NO ACTION,
    b_bbds text
    	REFERENCES commons.b_bbds (symbol)
        ON UPDATE CASCADE ON DELETE NO ACTION,
    ordinal integer
   	    REFERENCES commons.ordinal (symbol)
        ON UPDATE CASCADE ON DELETE NO ACTION,
    frequency integer,
    presence_only boolean,
    specimen text,
    spec_miguel integer
		REFERENCES specimens.specimens_miguel (coll_nr)
        ON UPDATE CASCADE ON DELETE RESTRICT,
    CONSTRAINT spec_miguel_unique UNIQUE (spec_miguel)
)

ALTER TABLE swea_dataveg.samples
    OWNER to miguel;
COMMENT ON TABLE swea_dataveg.samples
    IS 'Table containing single biodiversity records.';

COMMENT ON COLUMN swea_dataveg.samples."ReleveID"
    IS 'ID of plot observation.';
COMMENT ON COLUMN swea_dataveg.samples."TaxonUsageID"
    IS 'ID of taxon usage name.';
COMMENT ON COLUMN swea_dataveg.samples.veg_layer
    IS 'ID of layer in vegetation.';
COMMENT ON COLUMN swea_dataveg.samples.specimen
    IS 'ID of collected specimen.';
COMMENT ON COLUMN swea_dataveg.samples.sociability
    IS 'Sociability component in plant record.';
COMMENT ON COLUMN swea_dataveg.samples.misspelled_name
    IS 'Name recorded with typos or other spelling mistakes.';
COMMENT ON COLUMN swea_dataveg.samples.cover_percentage
    IS 'Species cover recorded in percentage.';
COMMENT ON COLUMN swea_dataveg.samples.br_bl
    IS 'Species cover recorded in Braun-Blanquet scale (old).';
COMMENT ON COLUMN swea_dataveg.samples.b_bbds
    IS 'Species cover recorded in Braun-Blanquet scale (new).';
COMMENT ON COLUMN swea_dataveg.samples.ordinal
    IS 'Species cover recorded in ordinal scale.';
COMMENT ON COLUMN swea_dataveg.samples.frequency
    IS 'Recorded frequency of individuals (or ramets).';
COMMENT ON COLUMN swea_dataveg.samples.presence_only
    IS 'Species recorded as presence-only.';
COMMENT ON COLUMN swea_dataveg.samples.spec_miguel
    IS 'Reference to specimens collected by Miguel Alvarez in the plot.';
    