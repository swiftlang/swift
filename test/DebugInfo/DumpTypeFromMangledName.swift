// RUN: %empty-directory(%t)

// %t.input: "A ---> B" ==> "A"
// RUN: sed -ne '/--->/s/ *--->.*$//p' < %S/Inputs/type-reconstr-names.txt > %t.input

// %t.check: "A ---> B" ==> "B"
// RUN: sed -ne '/--->/s/^.*---> *//p' < %S/Inputs/type-reconstr-names.txt > %t.check

// RUN: %target-build-swift -emit-executable %s -g -o %t/TypeReconstr -emit-module
// RUN: %lldb-moduleimport-test %t/TypeReconstr \
// RUN:   -type-from-mangled=%t.input > %t.output 2>&1
// RUN: diff %t.check %t.output

// REQUIRES: executable_test
func main() {
  struct Patatino {}
}

extension Collection where Element: Equatable {
  func split<C: Collection>(separatedBy separator: C) -> [SubSequence]
    where C.Element == Element {
      var results = [SubSequence]()
      return results
  }
}
