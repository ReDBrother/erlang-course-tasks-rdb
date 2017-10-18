-module(dna_test).
-include_lib("eunit/include/eunit.hrl").

rdna_test() ->
  Res1 = dna:rdna("GCTA"),
  Res2 = dna:rdna([g, c, t, a]),

  ?assertEqual("CGAU", Res1),
  ?assertEqual("CGAU", Res2).

cut_rdna_test() ->
  Res = dna:cut_rdna("AAGGTT", "AGG"),

  ?assertEqual("ATT", Res).
