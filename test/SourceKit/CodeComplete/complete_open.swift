struct Foo {
  func advancedFeatures(x: Int) {}
  var bigPower: Int = 0
}
func foo() {
  let x = Foo()

  x.
  // in comment
}

// RUN: %sourcekitd-test -req=complete.open -pos=8:5 %s -- %s | %FileCheck %s
// CHECK: key.results: [
// CHECK:   key.name: "advancedFeatures
// ...
// CHECK:   key.name: "bigPower"
// CHECK: ],
// CHECK: key.kind: source.lang.swift.codecomplete.group
// CHECK: key.name: ""

// RUN: %sourcekitd-test -req=complete.open -pos=8:5 %s -- %s \
// RUN:   == -req=complete.close -pos=8:5 %s -- %s \
// RUN:   == -req=complete.open -pos=8:5 %s -- %s \
// RUN:   == -req=complete.close -pos=8:5 %s -- %s > %t.close
// RUN: %FileCheck -check-prefix=CLOSE %s < %t.close
// CLOSE:   key.name: "advancedFeatures
// CLOSE:   key.name: "advancedFeatures

// RUN: %sourcekitd-test -req=complete.open -pos=8:5 %s -- %s \
// RUN:   == -req=complete.open -pos=7:1 %s -- %s > %t.open2diff
// RUN: %FileCheck -check-prefix=OPEN2DIFF %s < %t.open2diff
// OPEN2DIFF: key.name: "advancedFeatures
// OPEN2DIFF: key.name: "foo()
// OPEN2DIFF-NOT: key.name: "advancedFeatures

// RUN: not %sourcekitd-test -req=complete.open -pos=8:5 %s -- %s \
// RUN:   == -req=complete.open -pos=8:5 %s -- %s 2> %t.open2
// RUN: %FileCheck -check-prefix=OPEN2 %s < %t.open2
// OPEN2: error response (Request Failed): codecomplete.open: code completion session for '{{.*}}', {{.*}} already exists

// RUN: not %sourcekitd-test -req=complete.close -pos=7:1 %s -- %s 2> %t.closefail1
// RUN: %FileCheck -check-prefix=CLOSEFAIL %s < %t.closefail1
// RUN: not %sourcekitd-test -req=complete.open -pos=8:5 %s -- %s \
// RUN:   == -req=complete.close -pos=7:1 %s -- %s 2> %t.closefail2
// RUN: %FileCheck -check-prefix=CLOSEFAIL %s < %t.closefail2
// RUN: not %sourcekitd-test -req=complete.open -pos=8:5 %s -- %s \
// RUN:   == -req=complete.close -pos=8:5 %s -- %s \
// RUN:   == -req=complete.close -pos=8:5 %s -- %s 2> %t.closefail3
// RUN: %FileCheck -check-prefix=CLOSEFAIL %s < %t.closefail3
// CLOSEFAIL: error response (Request Failed): codecomplete.close: no code completion session for '{{.*}}'

// RUN: not %sourcekitd-test -req=complete.update -pos=7:1 %s -- %s 2> %t.updatefail1
// RUN: %FileCheck -check-prefix=UPDATEFAIL %s < %t.updatefail1
// RUN: not %sourcekitd-test -req=complete.open -pos=8:5 %s -- %s \
// RUN:   == -req=complete.update -pos=7:1 %s -- %s 2> %t.updatefail2
// RUN: %FileCheck -check-prefix=UPDATEFAIL %s < %t.updatefail2
// RUN: not %sourcekitd-test -req=complete.open -pos=8:5 %s -- %s \
// RUN:   == -req=complete.close -pos=8:5 %s -- %s \
// RUN:   == -req=complete.update -pos=8:5 %s -- %s 2> %t.updatefail3
// RUN: %FileCheck -check-prefix=UPDATEFAIL %s < %t.updatefail3
// UPDATEFAIL: error response (Request Failed): codecomplete.update: no code completion session for '{{.*}}'

// RUN: %sourcekitd-test -req=complete.open -pos=9:9 %s -- %s | %FileCheck %s -check-prefix=EMPTY
// EMPTY: key.results: [
// EMPTY-NEXT: ],
// EMPTY-NEXT: key.kind: source.lang.swift.codecomplete.group
// EMPTY-NEXT: key.name: ""
