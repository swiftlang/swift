// RUN: not %complete-test 2>&1 | FileCheck -check-prefix=MISSING-ALL %s
// MISSING-ALL: usage: complete-test -tok=A file
// MISSING-ALL: missing <source-file>

// RUN: not %complete-test %s 2>&1 | FileCheck -check-prefix=MISSING-TOK %s
// MISSING-TOK: missing -tok=

// RUN: not %complete-test -tok=NOPE %s 2>&1 | FileCheck -check-prefix=MISSING-TOK-IN %s
// MISSING-TOK-IN: cannot find code completion token in source file

// RUN: %complete-test -tok=INT_DOT %s | FileCheck -check-prefix=INT_DOT %s -strict-whitespace
// RUN: %complete-test -tok=ALL %s | FileCheck -check-prefix=ALL %s
// RUN: %complete-test -tok=DIFF -raw %s > %t.complete-test
// RUN: %sourcekitd-test -req=complete.open -pos=49:5  %s -- %s > %t.sourcekitd-test
// RUN: diff %t.complete-test %t.sourcekitd-test

func foo() {
  let x = 1
  x.#^INT_DOT^#
  // INT_DOT: {{^}}advanced(by:
  // INT_DOT: {{^}}bigEndian{{$}}
  // INT_DOT: {{^}}byteSwapped{{$}}
  // INT_DOT: {{^}}description{{$}}
  // INT_DOT: {{^}}distance(to: Int){{$}}
  // INT_DOT: {{^}}hashValue{{$}}
  // INT_DOT: {{^}}littleEndian{{$}}
  // INT_DOT: {{^}}predecessor(){{$}}
  // INT_DOT: {{^}}successor(){{$}}
  // INT_DOT: {{^}}toIntMax(){{$}}
}

func bar() {
  #^ALL^#
  // ALL: bar()
  // ALL: foo()
}

// A simple struct that will have a reliable sort order for diff'ing so that we
// can check that the -raw output from complete-test looks like
// sourcekitd-test.
struct ForDiff {
  let w: Int
  let x: String
  func y() {}
  func z() throws {}
}

func diff(x: ForDiff) {
  x.#^DIFF^#
}
