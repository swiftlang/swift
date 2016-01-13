// RUN: rm %t.txt
// RUN: echo '%S/Inputs/filelist-other.swift' >> %t.txt
// RUN: echo '%s' >> %t.txt
// RUN: echo '%S/../Inputs/empty.swift' >> %t.txt
// RUN: not %target-swift-frontend -parse -filelist %t.txt -primary-file %s 2>&1 | FileCheck %s
// RUN: not %target-swift-frontend -parse -filelist %t.txt 2>&1 | FileCheck %s

func test() {
  // Check with FileCheck because we want to see that this file is being
  // compiled.
  // CHECK: error: cannot convert value of type 'Bar' to specified type 'Foo'
  let x: Foo = other()
}