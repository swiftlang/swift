// RUN: %swift-syntax-test -input-source-filename %s -parse-gen > %t
// RUN: diff -u %s %t
// RUN: %swift-syntax-test -input-source-filename %s -parse-gen -print-node-kind > %t.withkinds
// RUN: diff -u %S/Outputs/round_trip_parse_gen.swift.withkinds %t.withkinds

class C {
  func bar(_ a: Int) {}
  func bar1(_ a: Float) -> Float { return -0.6 + 0.1 - 0.3 }
  func bar2(a: Int, b: Int, c:Int) -> Int { return 1 }
  func bar3(a: Int) -> Int { return 1 }
  func bar4(_ a: Int) -> Int { return 1 }
  func foo() {
    var a = /*comment*/"ab\(x)c"/*comment*/
    var b = /*comment*/+2/*comment*/
    bar(1)
    bar(+10)
    bar(-10)
    bar1(-1.1)
    bar1(1.1)
    var f = /*comments*/+0.1/*comments*/
    foo()
  }

  func foo1() {
    _ = bar2(a:1, b:2, c:2)
    _ = bar2(a:1 + 1, b:2 * 2 + 2, c:2 + 2)
    _ = bar2(a : bar2(a: 1, b: 2, c: 3), b: 2, c: 3)
    _ = bar3(a : bar3(a: bar3(a: 1)))
    _ = bar4(bar4(bar4(1)))
    _ = [1, 2, 3, 4]
    _ = [1:1, 2:2, 3:3, 4:4]
    _ = [bar3(a:1), bar3(a:1), bar3(a:1), bar3(a:1)]
    _ = ["a": bar3(a:1), "b": bar3(a:1), "c": bar3(a:1), "d": bar3(a:1)]
    foo(nil, nil, nil)
  }
  func boolAnd() -> Bool { return true && false }
  func boolOr() -> Bool { return true || false }

  func foo2() {
    _ = true ? 1 : 0
    _ = (true ? 1 : 0) ? (true ? 1 : 0) : (true ? 1 : 0)
    _ = (1, 2)
    _ = (first: 1, second: 2)
    _ = (1)
    _ = (first: 1)
    if !true {
      return
    }
  }

  func foo3() {
    _ = a.a.a
    _ = a.b
    _ = 1.a
    (1 + 1).a.b.foo
    _ = a as Bool || a as! Bool || a as? Bool
    _ = a is Bool
  }
}

typealias A = Any
typealias B = (Array<Array<Any>>.Element)
typealias C = [Int]
typealias D = [Int: String]
typealias E = Int?.Protocol
typealias F = [Int]!.Type

struct foo {
  struct foo {
    struct foo {
      func foo() {
      }
    }
  }
  struct foo {}
}

struct foo {
  @available(*, unavailable)
  struct foo {}
  public class foo {
    @available(*, unavailable)
    @objc(fooObjc)
    private static func foo() {}
  }
}

struct S<A, B, C, D> where A:B, B==C, A : C, B.C == D.A, A.B: C.D {}
