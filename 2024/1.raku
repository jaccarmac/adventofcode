my @problem = [Z] lines>>.comb(/ <digit>+ /);

say sum ([Z] @problem>>.sort).map({abs [-] $_});

my ($l, $r) = @problem;

say sum $l.map({$r.grep($_).elems * $_});
