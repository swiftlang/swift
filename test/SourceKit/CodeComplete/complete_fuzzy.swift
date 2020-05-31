struct S1 {
  func fooBarBaz() {}
  func fooBarTastic() {}
  func footastic() {}
}
func fooBarBaz() {}
func fooBarTastic() {}
func footastic() {}

// ===- Without a filter, we group.

// RUN: %complete-test %s -group=stems -tok=TOP_LEVEL_NO_FILTER | %FileCheck %s -check-prefix=TOP_LEVEL_NO_FILTER
// RUN: %complete-test %s -group=stems -fuzz -tok=TOP_LEVEL_NO_FILTER | %FileCheck %s -check-prefix=TOP_LEVEL_NO_FILTER
func test1() {
  #^TOP_LEVEL_NO_FILTER^#
// TOP_LEVEL_NO_FILTER: fooBar:
// TOP_LEVEL_NO_FILTER:   fooBarBaz()
// TOP_LEVEL_NO_FILTER:   fooBarTastic()
// TOP_LEVEL_NO_FILTER: footastic()
}
// RUN: %complete-test %s -group=stems -tok=S1_QUAL_NO_FILTER | %FileCheck %s -check-prefix=S1_QUAL_NO_FILTER
// RUN: %complete-test %s -group=stems -fuzz -tok=S1_QUAL_NO_FILTER | %FileCheck %s -check-prefix=S1_QUAL_NO_FILTER
func test2(x: S1) {
  x.#^S1_QUAL_NO_FILTER^#
// Without a filter, we group.
// S1_QUAL_NO_FILTER: fooBar:
// S1_QUAL_NO_FILTER:   fooBarBaz()
// S1_QUAL_NO_FILTER:   fooBarTastic()
// S1_QUAL_NO_FILTER: footastic()
}

// ===- Basic filter checks.
// RUN: %complete-test %s -no-fuzz -group=stems -tok=TOP_LEVEL_1 | %FileCheck %s -check-prefix=TOP_LEVEL_1_PREFIX
// RUN: %complete-test %s -fuzz -group=stems -tok=TOP_LEVEL_1 | %FileCheck %s -check-prefix=TOP_LEVEL_1_FUZZ
// RUN: %complete-test %s -fuzz -group=none -tok=TOP_LEVEL_1 | %FileCheck %s -check-prefix=TOP_LEVEL_1_FUZZ_NO_GROUP
func test3() {
  #^TOP_LEVEL_1,bar^#
// TOP_LEVEL_1_PREFIX-NOT: foo
// TOP_LEVEL_1_FUZZ: fooBar:
// TOP_LEVEL_1_FUZZ-NEXT: fooBarBaz()
// TOP_LEVEL_1_FUZZ-NEXT: fooBarTastic()
// TOP_LEVEL_1_FUZZ-NOT: footastic
// TOP_LEVEL_1_FUZZ_NO_GROUP-NOT: fooBar
// TOP_LEVEL_1_FUZZ_NO_GROUP: fooBarBaz()
// TOP_LEVEL_1_FUZZ_NO_GROUP: fooBarTastic()
// TOP_LEVEL_1_FUZZ_NO_GROUP-NOT: footastic
}

// RUN: %complete-test %s -group=stems -no-fuzz -tok=S1_QUAL_1 | %FileCheck %s -check-prefix=S1_QUAL_1_PREFIX
// RUN: %complete-test %s -group=stems -fuzz -tok=S1_QUAL_1 | %FileCheck %s -check-prefix=S1_QUAL_1_FUZZ
func test3() {
  #^S1_QUAL_1,foo,bar,tast,footast^#
// S1_QUAL_1_PREFIX-LABEL: Results for filterText: foo [
// S1_QUAL_1_PREFIX-NEXT:    fooBar:
// S1_QUAL_1_PREFIX-NEXT:      fooBarBaz()
// S1_QUAL_1_PREFIX-NEXT:      fooBarTastic()
// S1_QUAL_1_PREFIX-NEXT:    footastic()
// S1_QUAL_1_PREFIX-NEXT: ]
// S1_QUAL_1_PREFIX-LABEL: Results for filterText: bar [
// S1_QUAL_1_PREFIX-NEXT: ]
// S1_QUAL_1_PREFIX-LABEL: Results for filterText: tast [
// S1_QUAL_1_PREFIX-NEXT: ]
// S1_QUAL_1_PREFIX-LABEL: Results for filterText: footast [
// S1_QUAL_1_PREFIX-NEXT:    footastic()
// S1_QUAL_1_PREFIX-NEXT: ]

// S1_QUAL_1_FUZZ-LABEL: Results for filterText: foo [
// S1_QUAL_1_FUZZ:         fooBar:
// S1_QUAL_1_FUZZ-NEXT:      fooBarBaz()
// S1_QUAL_1_FUZZ-NEXT:      fooBarTastic()
// S1_QUAL_1_FUZZ-NEXT:    footastic()
// S1_QUAL_1_FUZZ: ]
// S1_QUAL_1_FUZZ-LABEL: Results for filterText: bar [
// S1_QUAL_1_FUZZ:         fooBar:
// S1_QUAL_1_FUZZ-NEXT:      fooBarBaz()
// S1_QUAL_1_FUZZ-NEXT:      fooBarTastic()
// S1_QUAL_1_FUZZ: ]
// S1_QUAL_1_FUZZ-LABEL: Results for filterText: tast [
// S1_QUAL_1_FUZZ-NEXT:     fooBarTastic()
// FIXME: should this be CHECK-NEXT? It's coming after a bunch of silly results
// from the stdlib.
// S1_QUAL_1_FUZZ:     footastic()
// S1_QUAL_1_FUZZ: ]
// S1_QUAL_1_FUZZ-LABEL: Results for filterText: footast [
// S1_QUAL_1_FUZZ-NEXT:    footastic()
// S1_QUAL_1_FUZZ-NEXT:    fooBarTastic()
// S1_QUAL_1_FUZZ: ]
}

// RUN: %complete-test %s -fuzz -tok=CONTEXT_SORT_1 | %FileCheck %s -check-prefix=CONTEXT_SORT_1
// RUN: %complete-test %s -fuzz -fuzzy-weight=1 -tok=CONTEXT_SORT_1 | %FileCheck %s -check-prefix=CONTEXT_SORT_4
// RUN: %complete-test %s -fuzz -fuzzy-weight=100 -tok=CONTEXT_SORT_1 | %FileCheck %s -check-prefix=CONTEXT_SORT_2
// RUN: %complete-test %s -fuzz -fuzzy-weight=10000 -no-inner-results -tok=CONTEXT_SORT_1 | %FileCheck %s -check-prefix=CONTEXT_SORT_3
let myVar = 1
struct Test4 {
  let myVarTest4 = 2
  func test4() {
    let myLocalVar = 2

    #^CONTEXT_SORT_1,myVa^#
// CONTEXT_SORT_1: Results for filterText: myVa [
// CONTEXT_SORT_1-NEXT: myVarTest4
// CONTEXT_SORT_1-NEXT: myLocalVar
// CONTEXT_SORT_1-NEXT: myVar

// CONTEXT_SORT_2: Results for filterText: myVa [
// CONTEXT_SORT_2-NEXT: myVarTest4
// CONTEXT_SORT_2-NEXT: myVar
// CONTEXT_SORT_2-NEXT: myLocalVar

// CONTEXT_SORT_3: Results for filterText: myVa [
// CONTEXT_SORT_3-NEXT: myVar
// CONTEXT_SORT_3-NEXT: myVarTest4
// CONTEXT_SORT_3-NEXT: myLocalVar

// CONTEXT_SORT_4: Results for filterText: myVa [
// CONTEXT_SORT_4-NEXT: myLocalVar
// CONTEXT_SORT_4-NEXT: myVarTest4
// CONTEXT_SORT_4-NEXT: myVar
  }
}

// RUN: %complete-test %s -fuzz -tok=DONT_FILTER_TYPES_1 | %FileCheck %s -check-prefix=DONT_FILTER_TYPES_1
struct Test5 {
  func dontFilterTypes(a: Int, b: Int, ccc: String) {}
  func dontFilterTypes(a: Int, b: Int, ddd: Float) {}
}
func test5(x: Test5) {
  x.#^DONT_FILTER_TYPES_1,ccc,str,ddd,flo^#
}
// DONT_FILTER_TYPES_1-LABEL: Results for filterText: ccc [
// DONT_FILTER_TYPES_1-NEXT:     dontFilterTypes(a: Int, b: Int, ccc: String)
// DONT_FILTER_TYPES_1-NEXT: ]
// DONT_FILTER_TYPES_1-LABEL: Results for filterText: str [
// DONT_FILTER_TYPES_1-NEXT: ]
// DONT_FILTER_TYPES_1-LABEL: Results for filterText: ddd [
// DONT_FILTER_TYPES_1-NEXT:     dontFilterTypes(a: Int, b: Int, ddd: Float)
// DONT_FILTER_TYPES_1-NEXT: ]
// DONT_FILTER_TYPES_1-LABEL: Results for filterText: flo [
// DONT_FILTER_TYPES_1-NEXT: ]

// RUN: %complete-test %s -fuzz -tok=MIN_LENGTH_1 | %FileCheck %s -check-prefix=MIN_LENGTH_1
func test6(x: S1) {
  x.#^MIN_LENGTH_1,f,o,b^#
}
// MIN_LENGTH_1-LABEL: Results for filterText: f [
// MIN_LENGTH_1-NEXT:   fooBarBaz()
// MIN_LENGTH_1-NEXT:   footastic()
// MIN_LENGTH_1-NEXT:   fooBarTastic()
// MIN_LENGTH_1-NEXT: ]
// MIN_LENGTH_1-LABEL: Results for filterText: o [
// MIN_LENGTH_1-NEXT: ]
// MIN_LENGTH_1-LABEL: Results for filterText: b [
// MIN_LENGTH_1-NEXT: ]

// RUN: %complete-test %s -fuzz -tok=MAP | %FileCheck %s -check-prefix=MAP
protocol P {
  func map()
}
extension P {
  func map() {}
}
struct Arr : P {
  func withUnsafeMutablePointer() {}
}
func test7(x: Arr) {
  x.#^MAP,ma,map^#
}
// MAP: Results for filterText: ma [
// MAP-NEXT: map()
// MAP-NEXT: withUnsafeMutablePointer()
// MAP-NEXT: ]
// MAP-LABEL: Results for filterText: map [
// MAP-NEXT: map()
// MAP-NEXT: withUnsafeMutablePointer()
// MAP-NEXT: ]
