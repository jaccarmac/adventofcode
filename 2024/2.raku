my @problem = lines.map({.words});

sub gen-diffs(($last, $diffs), $next) {
  ($next, (|$diffs, $next - $last))
}

say elems @problem.grep: {
  -> ($l, $d) {
    so $d[2..*].all < 0 || so $d[2..*].all > 0 and so $d[2..*]».abs.all ≤ 3
  }(reduce &gen-diffs, (0, 0), |$_)
}
