CREATE TABLE "<schema>".taxon_names (
    taxon_usage_id SERIAL PRIMARY KEY,
    usage_name text NOT NULL,
    author_name text
);
COMMENT ON TABLE "<schema>".taxon_names
    IS 'Collection of names for syntaxa.';
COMMENT ON COLUMN "<schema>".taxon_names.taxon_usage_id
    IS 'ID of syntaxon usage name.';
COMMENT ON COLUMN "<schema>".taxon_names.usage_name
    IS 'Syntaxon usage name.';
COMMENT ON COLUMN "<schema>".taxon_names.author_name
    IS 'Author of the staxon usage name.';
