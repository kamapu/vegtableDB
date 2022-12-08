CREATE TABLE "<schema>".taxonomies (
    taxonomy text PRIMARY KEY,
    description text,
    bibtexkey text REFERENCES <schema_bib_references>.main_table(bibtexkey)
        ON UPDATE CASCADE
);
COMMENT ON TABLE "<schema>".taxonomies
    IS 'List of taxonomic lists used as top taxon views.';
COMMENT ON COLUMN "<schema>".taxonomies.taxonomy
    IS 'Short name of taxonomic list (primary key).';
COMMENT ON COLUMN "<schema>".taxonomies.description
    IS 'Description of taxonomic list.';
COMMENT ON COLUMN "<schema>".taxonomies.bibtexkey
    IS 'Key of reference used as top taxon view (foreign key).';
