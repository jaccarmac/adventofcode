my @problem = lines».words;

say elems @problem.grep: {
  given reduce -> ($l, $d), $n {$n, (|$d, $n - $l)}, (0, ()), |$_
  -> ($, $d) {
    so $d[1..*].all < 0 || so $d[1..*].all > 0 and so $d[1..*]».abs.all ≤ 3
  }
}
