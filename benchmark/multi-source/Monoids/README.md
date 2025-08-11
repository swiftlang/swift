# Monoids Benchmark

This benchmark solves the "word problem" in a bunch of monoids simultaneously, using Swift concurrency (or really, just `Task`). It exercises the standard library data structures heavily. You can also run "sh compile.sh" inside the source directory to build a standalone binary separately from the benchmark harness. The standalone binary prints results to standard output.

More specifically, this program enumerates two-generator two-relation monoid presentations up to length 10, and then applies the Knuth-Bendix algorithm to each one:

    <a,b|u=v,w=x>  where |u| + |v| + |w| + |x| <= 10

This takes a few seconds to finish and solves all but three instances. The three it cannot solve are:

    <a,b|aaa=a,abba=bb>
    <a,b|bab=aaa,bbbb=1>
    <a,b|aaaa=1,abbba=b>

In addition to Knuth-Bendix completion, there are some other interesting algorithms here as well:
- Shortlex order with arbitrary permutation of alphabet
- Wreath product order (also known as recursive path order) with arbitrary degree mapping
- Enumerating all words, permutations, monoid presentations
- Inverse of a permutation
- Computing number of irreducible words in complete presentation (= cardinality of presented monoid) with finite state automata

The Knuth-Bendix implementation is pretty fast. It uses a trie to speed up reduction and finding overlaps.

The main "entry point" is `func run()` in Monoids.swift.
