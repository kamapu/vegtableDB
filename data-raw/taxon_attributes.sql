CREATE TABLE "<schema>".taxon_attributes (
    taxon_concept_id integer NOT NULL
        REFERENCES "<schema>".taxon_concepts(taxon_concept_id)
);
COMMENT ON TABLE "<schema>".taxon_attributes
    IS 'Attributes for formations in the Braun-Blanque approach (Sudamerica).';
COMMENT ON COLUMN "<schema>".taxon_attributes.taxon_concept_id
    IS 'ID for formation concepts (foreign key).';
