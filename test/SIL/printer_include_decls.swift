// RUN: rm -f %t.*
// RUN: %target-swift-frontend -emit-sil %s -o %t.sil
// RUN: FileCheck --input-file=%t.sil %s
// RUN: %target-swift-frontend -emit-silgen %t.sil -module-name=printer_include_decl | FileCheck %s

var x: Int
// CHECK: var x: Int

class Foo {
// FIXME: The constructors and destructors without bodies cannot be parsed.
  init(i: Int) {
    self.x = i
  }
// CHECK: init(i: Int)

  deinit { m() }
// CHECK: deinit

  subscript(x: Int, y: Int) -> Int {
    get {
      return 0
    }
    set {}
  }
// CHECK: subscript (x: Int, y: Int) -> Int

  final var x : Int
// CHECK: var x: Int

  final var y : Int {
    get {
      return 5
    }
  }
// CHECK: var y: Int

  func m() {}
// CHECK: func m()
}

func bar(x: Foo) -> Int {
  return x.x
}
// CHECK: func bar(x: Foo) -> Int
