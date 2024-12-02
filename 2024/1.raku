my $problem = [Z] lines>>.comb(/ <digit>+ /);

say sum ([Z] $problem>>.sort).map({abs [-] $_})
