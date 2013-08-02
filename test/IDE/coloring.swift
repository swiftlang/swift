// RUN: %swift-ide-test -syntax-coloring -source-filename %s | FileCheck %s
// FIXME: Not quite right yet.

// CHECK: [[@LINE+1]]: { (Keyword, 1:6) }
struct S {
 // CHECK-NEXT: [[@LINE+1]]: { (Keyword, 2:3) }
 var x: Int
}

// CHECK-NEXT: [[@LINE+1]]: { (Keyword, 1:5) }
class MyCls {
    // CHECK-NEXT: [[@LINE+1]]: { (Keyword, 5:3) }
    var www : Int
    // CHECK-NEXT: [[@LINE+1]]: { (Keyword, 5:4) }
    func foo(x : Int) {}
}

// CHECK-NEXT: [[@LINE+1]]: { (CommentBlock, 1:23) }
/*
 foo is the best
 */
// CHECK-NEXT: [[@LINE+1]]: { (Keyword, 1:4) }
func foo(n : Float) -> Int {
    // CHECK-NEXT: [[@LINE+1]]: { (Keyword, 5:3) }
    var q = MyCls()
    // CHECK-NEXT: [[@LINE+1]]: { (Keyword, 5:3) (String, 14:5) }
    var ee = "yoo";
    // CHECK-NEXT: [[@LINE+1]]: { (Keyword, 5:6) (Integer, 12:6) }
    return 100009
}

// CHECK-NEXT: [[@LINE+1]]: { (Keyword, 1:8) }
protocol Prot {}
// CHECK-NEXT: [[@LINE+1]]: { (Keyword, 1:8) }
protocol Prot2 {}

// CHECK-NEXT: [[@LINE+1]]: { (Keyword, 1:4) (Keyword, 21:5) (Keyword, 44:1) }
func genFn<T : Prot where T.Blarg : Prot2>(_ : T) -> Int {}

// CHECK-NEXT: [[@LINE+1]]: { (Keyword, 1:4) }
func f(x: Int) -> Int {
  // CHECK-NEXT: [[@LINE+1]]: { (CommentLine, 3:36) }
  // string interpolation is the best
  // CHECK-NEXT: [[@LINE+1]]: { (String, 3:62) }
  "This is string \(genFn({(a:Int) -> Int in a})) interpolation"
}

// CHECK-NEXT: [[@LINE+1]]: { (Keyword, 1:4) }
func bar(x: Int) -> Int {
  foo(Float())
}
