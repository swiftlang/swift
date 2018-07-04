struct Foo {
  func aaa() {}
  func aab() {}
  func abc() {}
  func b() {}
  func c() {}
}

func foo() {
  let x = Foo()
  x.#^FOO,,a,aa,ab,abc,abcd^#
}

// XFAIL: broken_std_regex
// RUN: %sourcekitd-test -req=complete.open -pos=11:5 %s -- %s > %t.all
// RUN: %FileCheck -check-prefix=CHECK-ALL %s < %t.all
// CHECK-ALL: key.name: "aaa
// CHECK-ALL: key.name: "aab
// CHECK-ALL: key.name: "abc
// CHECK-ALL: key.name: "b
// CHECK-ALL: key.name: "c

// RUN: %sourcekitd-test -req=complete.open -pos=11:5 \
// RUN:   -req-opts=filtertext=a %s -- %s > %t.a
// RUN: %FileCheck -check-prefix=CHECKA %s < %t.a

// CHECKA-NOT: key.name
// CHECKA: key.name: "aaa
// CHECKA: key.name: "aab
// CHECKA: key.name: "abc
// CHECKA-NOT: key.name: "b
// CHECKA-NOT: key.name: "c

// RUN: %sourcekitd-test -req=complete.open -pos=11:5 %s -- %s \
// RUN:   == -req=complete.update -pos=11:5 -req-opts=filtertext=a %s -- %s > %t.both
// RUN: cat %t.all %t.a > %t.both.check
// RUN: diff -u %t.both %t.both.check

// RUN: %sourcekitd-test -req=complete.open -pos=11:5 \
// RUN:   -req-opts=filtertext=b %s -- %s > %t.b
// RUN: %FileCheck -check-prefix=CHECKB %s < %t.b

// CHECKB-NOT: key.name
// CHECKB: key.name: "b
// CHECKB-NOT: key.name: "c

// RUN: %sourcekitd-test -req=complete.open -pos=11:5 \
// RUN:   -req-opts=filtertext=c %s -- %s > %t.c
// RUN: %FileCheck -check-prefix=CHECKC %s < %t.c

// CHECKC-NOT: key.name
// CHECKC: key.name: "c

// RUN: %sourcekitd-test -req=complete.open -pos=11:5 \
// RUN:   -req-opts=filtertext=d %s -- %s > %t.d
// RUN: %FileCheck -check-prefix=CHECKD %s < %t.d

// CHECKD-NOT: key.name
// CHECKD: ],
// CHECKD-NEXT: key.kind: source.lang.swift.codecomplete.group
// CHECKD-NEXT: key.name: ""
// CHECKD-NEXT: key.nextrequeststart: 0
// CHECKD-NEXT: }


// RUN: %complete-test -tok=FOO %s | %FileCheck %s
// CHECK-LABEL: Results for filterText: [
// CHECK-NEXT:   aaa()
// CHECK-NEXT:   aab()
// CHECK-NEXT:   abc()
// CHECK-NEXT:   b()
// CHECK-NEXT:   c()
// CHECK-NEXT:   self
// CHECK-NEXT: ]
// CHECK-LABEL: Results for filterText: a [
// CHECK-NEXT:   aaa()
// CHECK-NEXT:   aab()
// CHECK-NEXT:   abc()
// CHECK-NEXT: ]
// CHECK-LABEL: Results for filterText: aa [
// CHECK-NEXT:   aaa()
// CHECK-NEXT:   aab()
// CHECK-NEXT: ]
// CHECK-LABEL: Results for filterText: abc [
// CHECK-NEXT:   abc()
// CHECK-NEXT: ]
// CHECK-LABEL: Results for filterText: abcd [
// CHECK-NEXT: ]

struct A {}
func over() {}
func overload() {}
func overload(_ x: A) {}

func test() {
  #^OVER,over,overload,overloadp^#
}
// Don't create empty groups, or groups with one element
// RUN: %complete-test -group=overloads -tok=OVER %s | %FileCheck -check-prefix=GROUP %s
// GROUP-LABEL: Results for filterText: over [
// GROUP: overload:
// GROUP-NEXT:   overload()
// GROUP-NEXT:   overload(x: A)
// GROUP: ]
// GROUP-LABEL: Results for filterText: overload [
// GROUP-NEXT:   overload()
// GROUP-NEXT:   overload(x: A)
// GROUP-NEXT: ]
// GROUP-LABEL: Results for filterText: overloadp [
// GROUP-NEXT: ]

struct UnnamedArgs {
  func dontMatchAgainst(_ unnamed: Int, arguments: Int, _ unnamed2:Int) {}
  func test() {
    self.#^UNNAMED_ARGS_0,dont,arguments,unnamed^#
  }
}

// RUN: %complete-test -tok=UNNAMED_ARGS_0 %s | %FileCheck %s -check-prefix=UNNAMED_ARGS_0
// UNNAMED_ARGS_0: Results for filterText: dont [
// UNNAMED_ARGS_0-NEXT:   dontMatchAgainst(unnamed: Int, arguments: Int, unnamed2: Int)
// UNNAMED_ARGS_0-NEXT: ]
// UNNAMED_ARGS_0-NEXT: Results for filterText: arguments [
// UNNAMED_ARGS_0-NEXT:   dontMatchAgainst(unnamed: Int, arguments: Int, unnamed2: Int)
// UNNAMED_ARGS_0-NEXT: ]
// UNNAMED_ARGS_0-NEXT: Results for filterText: unnamed [
// UNNAMED_ARGS_0-NEXT: ]
