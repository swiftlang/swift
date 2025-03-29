// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(Swiftskell)) \
// RUN:    -module-name Swiftskell \
// RUN:    -emit-module      \
// RUN:    -emit-module-path %t/Swiftskell.swiftmodule \
// RUN:    -enable-library-evolution \
// RUN:    -parse-as-library \
// RUN:    %S/../Inputs/Swiftskell.swift \
// RUN:   -enable-experimental-feature SuppressedAssociatedTypes 

// RUN: %target-build-swift -I%t -L%t -lSwiftskell -parse-as-library %s \
// RUN:                     -module-name E -o %t/E %target-rpath(%t)
// RUN: %target-codesign %t/E
// RUN: %target-codesign %t/%target-library-name(Swiftskell)
// RUN: %target-run %t/E %t/%target-library-name(Swiftskell) \
// RUN:     | %FileCheck %s --implicit-check-not destroy

// RUN: %target-build-swift -O -I%t -L%t -lSwiftskell -parse-as-library %s \
// RUN:                     -module-name Opt -o %t/Opt %target-rpath(%t)
// RUN: %target-codesign %t/Opt
// RUN: %target-run %t/Opt %t/%target-library-name(Swiftskell) | %FileCheck %s --implicit-check-not destroy

// REQUIRES: executable_test
// REQUIRES: swift_feature_SuppressedAssociatedTypes

// Temporarily disable for back-deployment (rdar://128544927)
// UNSUPPORTED: back_deployment_runtime

// And on older OS (rdar://132936383)
// UNSUPPORTED: use_os_stdlib

import Swiftskell

/// Basic noncopyable type for testing.
struct File: ~Copyable, Show {
  let id: Int
  init(_ id: Int) {
    self.id = id
  }
  func show() -> String { return id.show() }

  deinit { print("destroying file \(id)") }
}


@main
struct Main {
  static func main() {
    testListBasic()
  }
}

func testListBasic() {
  var items = List<File>(length: 5) { .init($0) }
  print(items.show())  // CHECK: [0, 1, 2, 3, 4, ]
  check(items.length() == 5)
  check(!items.isEmpty)

  items = List<File>(length: 5) { .init($0) }
  // CHECK: destroying file 4
  // CHECK: destroying file 3
  // CHECK: destroying file 2
  // CHECK: destroying file 1
  // CHECK: destroying file 0

  items = items.reverse()
  check(items.length() == 5)
  print(items.show())  // CHECK: [4, 3, 2, 1, 0, ]

  items = .empty
  // CHECK: destroying file 0
  // CHECK: destroying file 1
  // CHECK: destroying file 2
  // CHECK: destroying file 3
  // CHECK: destroying file 4

  check(items.length() == 0)
  check(items.isEmpty)

  let nums = List<Int>().push(7).push(7).push(3)
  print(nums.show()) // CHECK: [7, 7, 3, ]


}
