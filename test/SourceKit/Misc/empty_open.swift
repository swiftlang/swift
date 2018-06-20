let a = 12

// RUN: not %sourcekitd-test \
// RUN:   -req=open %s -- %s == \
// RUN:   -req=open '' \
// RUN:   2>&1 | %FileCheck %s

// CHECK: empty 'key.name'
