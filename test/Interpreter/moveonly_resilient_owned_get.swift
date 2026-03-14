// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module-path %t -enable-library-evolution -module-name moveonly_resilient_type -parse-as-library %S/Inputs/moveonly_resilient_type.swift -c -o %t/moveonly_resilient_type.o
// RUN: %target-build-swift -enable-library-evolution -module-name moveonly_resilient_type -parse-as-library %S/Inputs/moveonly_resilient_type.swift -c -o %t/moveonly_resilient_type.o
// RUN: %target-build-swift -o %t/a.out -I %t %s %t/moveonly_resilient_type.o
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

import moveonly_resilient_type

func concrete(_ g: GiverImpl) {
  g.thing.consume()
}


public func generic(_ g: some Giver) {
  g.thing.consume()
}

func main() {
  let g = GiverImpl()
  print("starting") // CHECK: starting

  concrete(g) // CHECK: HasDeinit.deinit
  concrete(g) // CHECK: HasDeinit.deinit
  generic(g) // CHECK: HasDeinit.deinit
  generic(g) // CHECK: HasDeinit.deinit

  print("finished")  // CHECK: finished
}
main()
