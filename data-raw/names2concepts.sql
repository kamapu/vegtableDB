CREATE TABLE "<schema>".names2concepts (
	tax_id SERIAL PRIMARY KEY,
    taxon_usage_id integer NOT NULL
        REFERENCES "<schema>".taxon_names(taxon_usage_id),
    taxon_concept_id integer NOT NULL
        REFERENCES "<schema>".taxon_concepts(taxon_concept_id),
    name_status text NOT NULL DEFAULT 'accepted'
);
COMMENT ON COLUMN "<schema>".names2concepts.tax_id
    IS 'Connector for observed occurrences of a taxon (primary key).';
COMMENT ON TABLE "<schema>".names2concepts
    IS 'Assignment of names to taxa and their status.';
COMMENT ON COLUMN "<schema>".names2concepts.taxon_usage_id
    IS 'ID of taxon usage name.';
COMMENT ON COLUMN "<schema>".names2concepts.taxon_concept_id
    IS 'ID of taxon concept.';
COMMENT ON COLUMN "<schema>".names2concepts.name_status
    IS 'Status of the name in the taxon.';
