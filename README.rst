Hot Tub
-------

Hot Tub is a permanent erlang worker pool. It keeps some number of worker
processes alive at all times and then attempts to load balance requests across
the set of workers by picking the one with the fewest current users.

Primarily I use this as a database worker pool though it certainly could have
other uses.

