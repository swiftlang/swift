// RUN: %sourcekitd-test -req=related-idents -pos=2:10 %S/Inputs/implicit-vis/a.swift \
// RUN:     -- %S/Inputs/implicit-vis/a.swift %S/Inputs/implicit-vis/b.swift -o implicit_vis.o | %FileCheck -check-prefix=CHECK1 %s

// CHECK1: START RANGES
// CHECK1: 2:10 - 1
// CHECK1: 3:10 - 1
// CHECK1: END RANGES
