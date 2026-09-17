// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -target %target-swift-6.2-abi-triple -verify %s
// RUN: %target-swift-emit-silgen -target %target-swift-6.2-abi-triple -verify %s

struct A {
  static let a: InlineArray = [1]

  static func foo() {
    a.span.withUnsafeBufferPointer({ buffer in
      print("\(buffer.baseAddress!)")
    })
  }
}
