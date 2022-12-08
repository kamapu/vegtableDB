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


CREATE TABLE "<schema>".taxon_levels (
    rank text PRIMARY KEY,
    rank_idx integer NOT NULL
);


COMMENT ON TABLE "<schema>".taxon_levels
    IS 'Syntax levels in the Braun-Blanquet system.';


COMMENT ON COLUMN "<schema>".taxon_levels."rank"
    IS 'Name of the syntaxonomic rank.';


COMMENT ON COLUMN "<schema>".taxon_levels.rank_idx
    IS 'Numeric index corresponding to the level of the rank from the bottom to the top.';


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


CREATE TABLE "<schema>".taxon_attributes (
    taxon_concept_id integer NOT NULL
        REFERENCES "<schema>".taxon_concepts(taxon_concept_id)
);


COMMENT ON TABLE "<schema>".taxon_attributes
    IS 'Attributes for formations in the Braun-Blanque approach (Sudamerica).';


COMMENT ON COLUMN "<schema>".taxon_attributes.taxon_concept_id
    IS 'ID for formation concepts (foreign key).';


