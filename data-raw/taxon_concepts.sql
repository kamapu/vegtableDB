CREATE TABLE "<schema>".taxon_concepts (
    taxon_concept_id SERIAL PRIMARY KEY,
    parent_id integer REFERENCES "<schema>".taxon_concepts(taxon_concept_id),
    rank text REFERENCES "<schema>".taxon_levels("rank") ON UPDATE CASCADE,
    view_key text REFERENCES "<schema_references>".main_table(bibtexkey),
    top_view text NOT NULL REFERENCES "<schema>".taxonomies(taxonomy)
);
COMMENT ON TABLE "<schema>".taxon_concepts
    IS 'Syntaxonomic concepts.';
COMMENT ON COLUMN "<schema>".taxon_concepts.taxon_concept_id
    IS 'IDs for syntaxonomic concepts (primary key).';
COMMENT ON COLUMN "<schema>".taxon_concepts.parent_id
    IS 'IDs for the respective parent concepts.';
COMMENT ON COLUMN "<schema>".taxon_concepts."rank"
    IS 'Syntaxonomic rank of the concept.';
COMMENT ON COLUMN "<schema>".taxon_concepts.view_key
    IS 'Reference to the source for cincumscription of the concept (foreign key).';
COMMENT ON COLUMN "<schema>".taxon_concepts.top_view
    IS 'Reference to the source for the whole taxonomy (foreign key).';
