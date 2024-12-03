my @problem = lines».words».Array;

sub reactorok($r) {
  given reduce -> ($l, $d), $n {$n, (|$d, $n - $l)}, (0, ()), |$r
  -> ($, $d) {
    so $d[1..*].all < 0 || so $d[1..*].all > 0 and so $d[1..*]».abs.all ≤ 3
  }
}

say elems @problem.grep: &reactorok;
say elems @problem.grep: {so reactorok any $^a.combinations: $^a.elems - 1};
