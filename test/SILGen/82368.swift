// RUN: %target-swift-emit-silgen -disable-availability-checking -verify %s

struct A {
  static let a: InlineArray = [1]

  static func foo() {
    a.span.withUnsafeBufferPointer({ buffer in
      print("\(buffer.baseAddress!)")
    })
  }
}
