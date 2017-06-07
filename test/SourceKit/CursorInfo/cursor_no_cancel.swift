func myFunc() {
  _ = 1
}

// Perform 8 concurrent cursor infos, which is often enough to cause
// contention.  We disable printing the requests to minimize delay.

// RUN: %sourcekitd-test \
// RUN:      -async -dont-print-request -cancel-on-subsequent-request=0 -req=cursor -pos=1:6 %s -- %s \
// RUN:   == -async -dont-print-request -cancel-on-subsequent-request=0 -req=cursor -pos=1:6 %s -- %s \
// RUN:   == -async -dont-print-request -cancel-on-subsequent-request=0 -req=cursor -pos=1:6 %s -- %s \
// RUN:   == -async -dont-print-request -cancel-on-subsequent-request=0 -req=cursor -pos=1:6 %s -- %s \
// RUN:   == -async -dont-print-request -cancel-on-subsequent-request=0 -req=cursor -pos=1:6 %s -- %s \
// RUN:   == -async -dont-print-request -cancel-on-subsequent-request=0 -req=cursor -pos=1:6 %s -- %s \
// RUN:   == -async -dont-print-request -cancel-on-subsequent-request=0 -req=cursor -pos=1:6 %s -- %s \
// RUN:   == -async -dont-print-request -cancel-on-subsequent-request=0 -req=cursor -pos=1:6 %s -- %s 2>&1 \
// RUN:   | %FileCheck %s -implicit-check-not='Request Cancel'

// CHECK: source.lang.swift.decl.function.free
// CHECK: source.lang.swift.decl.function.free
// CHECK: source.lang.swift.decl.function.free
// CHECK: source.lang.swift.decl.function.free
// CHECK: source.lang.swift.decl.function.free
// CHECK: source.lang.swift.decl.function.free
// CHECK: source.lang.swift.decl.function.free
// CHECK: source.lang.swift.decl.function.free

// RUN: %sourcekitd-test \
// RUN:      -async -dont-print-request -cancel-on-subsequent-request=0 -req=range -pos=2:3 -length=5 %s -- %s \
// RUN:   == -async -dont-print-request -cancel-on-subsequent-request=0 -req=range -pos=2:3 -length=5 %s -- %s \
// RUN:   == -async -dont-print-request -cancel-on-subsequent-request=0 -req=range -pos=2:3 -length=5 %s -- %s \
// RUN:   == -async -dont-print-request -cancel-on-subsequent-request=0 -req=range -pos=2:3 -length=5 %s -- %s \
// RUN:   == -async -dont-print-request -cancel-on-subsequent-request=0 -req=range -pos=2:3 -length=5 %s -- %s \
// RUN:   == -async -dont-print-request -cancel-on-subsequent-request=0 -req=range -pos=2:3 -length=5 %s -- %s \
// RUN:   == -async -dont-print-request -cancel-on-subsequent-request=0 -req=range -pos=2:3 -length=5 %s -- %s \
// RUN:   == -async -dont-print-request -cancel-on-subsequent-request=0 -req=range -pos=2:3 -length=5 %s -- %s 2>&1 \
// RUN:   | %FileCheck %s -check-prefix=RANGE -implicit-check-not='Request Cancel'

// RANGE: source.lang.swift.range.singleexpression
// RANGE: source.lang.swift.range.singleexpression
// RANGE: source.lang.swift.range.singleexpression
// RANGE: source.lang.swift.range.singleexpression
// RANGE: source.lang.swift.range.singleexpression
// RANGE: source.lang.swift.range.singleexpression
// RANGE: source.lang.swift.range.singleexpression
// RANGE: source.lang.swift.range.singleexpression
