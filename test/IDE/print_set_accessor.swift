public struct SomeType {
  public private(set) var privateSetter: Int
}
// CHECK: public private(set) var privateSetter: Int

// RUN: %target-swift-ide-test -print-swift-file-interface -source-filename %s | %FileCheck %s
