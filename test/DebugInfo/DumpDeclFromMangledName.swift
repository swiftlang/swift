// RUN: %empty-directory(%t)

// %t.input: "A ---> B" ==> "A"
// RUN: sed -ne '/--->/s/ *--->.*$//p' < %S/Inputs/decl-reconstr-names.txt > %t.input

// %t.check: "A ---> B" ==> "B"
// RUN: sed -ne '/--->/s/^.*---> *//p' < %S/Inputs/decl-reconstr-names.txt > %t.check

// RUN: %target-build-swift -emit-executable %s -g -o %t/DeclReconstr -emit-module
// RUN: %lldb-moduleimport-test %t/DeclReconstr \
// RUN:   -decl-from-mangled=%t.input > %t.output 2>&1
// RUN: diff %t.check %t.output

// REQUIRES: executable_test
struct S {
  init() {
  }
}

func patatino() -> Int {
  let s = S()
  return 0
}

patatino()
