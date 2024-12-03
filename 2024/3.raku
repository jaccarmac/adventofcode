$_ = ($*IN.slurp ~~ m:g/ mul\((\d+)\,(\d+)\) || don\'t\(\) || do\(\) /);

say [+] $_.map: {$_[0] * $_[1] if $_.elems}

say (reduce -> ($go, $s), $m {
  given $m {
    when "don't()" {False, $s}
    when "do()" {True, $s}
    default {next unless $go; $go, $s + $_[0] * $_[1]}
  }
}, (True, 0), |$_)[1]
