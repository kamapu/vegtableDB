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
