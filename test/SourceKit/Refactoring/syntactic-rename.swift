struct AStruct {
/**
    A description

    - parameter a: The first param
*/
func foo(a: Int) -> Int {
    let z = a + 1
    #if true
        return z
    #else
        return z + 1
    #endif
}
}
let aStruct = AStruct()
let x = aStruct.foo(a: 2)
let y = AStruct.foo(aStruct)(a: 3)
print(x + 2)
print(y + 1)

let s = "a foo is here"
#selector(AStruct.foo(a:))
#selector(AStruct.foo())
#selector(AStruct.foo)
let y = "before foo \(foo(a:1)) foo after foo"

func bar(a/* a comment */: Int, b c: Int, _: Int, _ d: Int) {}
bar(a: 1, b: 2, 3, 4)

/// a comment named example
func example() {}
/// another comment named example
class Example {}

class Init {
  init(x: Int) {}
}
_ = Init(x: 1)

enum MyEnum {
  case first
  case second(Int)
  case third(x: Int)
  case fourth(x: Int, y: Int, Int)
}

let first = MyEnum.first
let second = MyEnum.second(2)
let _ = MyEnum.second(_: 2)
let third = MyEnum.third(x: 1)
let fourth = MyEnum.fourth(x: 1, y: 2, 3)

switch fourth {
case .first:
  print(1)
case .second(_: let x):
  print(x)
case .third(x: let x):
  print(x)
case .fourth(let x, y: let y, _: let z):
  print(x + y + z)
}

struct Memberwise1 {
  let x: Int
  let y = 0
}

struct Memberwise2 {
  let m: Memberwise1
  let n: Memberwise1; subscript(x: Int) -> Int { return x }
}

_ = Memberwise2(m: Memberwise1(x: 1), n: Memberwise1.init(x: 2))[1]

protocol Layer {
  associatedtype Content
}
struct MultiPaneLayout<A: Layer, B: Layer>: Layer where A.Content == B.Content{
  typealias Content = Int
}

protocol P {}
struct S {
    subscript<K: P>(key: K) -> Int {
        return 0
    }
}
protocol Q {}
func genfoo<T: P, U, V where U: P>(x: T, y: U, z: V, a: P) -> P where V: P {
  fatalError()
}

_ = Memberwise1(x: 2)
_ = Memberwise1.init(x: 2)
_ = Memberwise2.init(m: Memberwise1(x: 2), n: Memberwise1(x: 34))
_  = " this init is init "
// this init is init too

#if NOTDEFINED
_ = Memberwise1(x: 2)
_ = Memberwise1.init(x: 2)
_ = Memberwise2.init(m: 2, n: Memberwise1(x: 34))
#endif

// RUN: %empty-directory(%t.result)
// RUN: %sourcekitd-test -req=syntactic-rename -rename-spec %S/syntactic-rename/x.in.json %s >> %t.result/x.expected
// RUN: diff -u %S/syntactic-rename/x.expected %t.result/x.expected
// RUN: %sourcekitd-test -req=syntactic-rename -rename-spec %S/syntactic-rename/z.in.json %s >> %t.result/z.expected
// RUN: diff -u %S/syntactic-rename/z.expected %t.result/z.expected
// RUN: %sourcekitd-test -req=syntactic-rename -rename-spec %S/syntactic-rename/foo.in.json %s >> %t.result/foo_arity1.expected
// RUN: diff -u %S/syntactic-rename/foo_arity1.expected %t.result/foo_arity1.expected
// RUN: %sourcekitd-test -req=syntactic-rename -rename-spec %S/syntactic-rename/foo_remove.in.json %s >> %t.result/foo_remove.expected
// RUN: diff -u %S/syntactic-rename/foo_remove.expected %t.result/foo_remove.expected
// RUN: %sourcekitd-test -req=syntactic-rename -rename-spec %S/syntactic-rename/bar.in.json %s >> %t.result/bar.expected
// RUN: diff -u %S/syntactic-rename/bar.expected %t.result/bar.expected
// RUN: %sourcekitd-test -req=syntactic-rename -rename-spec %S/syntactic-rename/bar_add_param.in.json %s >> %t.result/bar_add_param.expected
// RUN: diff -u %S/syntactic-rename/bar_add_param.expected %t.result/bar_add_param.expected
// RUN: %sourcekitd-test -req=syntactic-rename -rename-spec %S/syntactic-rename/bar_drop_param.in.json %s >> %t.result/bar_drop_param.expected
// RUN: diff -u %S/syntactic-rename/bar_drop_param.expected %t.result/bar_drop_param.expected
// RUN: %sourcekitd-test -req=syntactic-rename -rename-spec %S/syntactic-rename/comment.in.json %s >> %t.result/comment.expected
// RUN: diff -u %S/syntactic-rename/comment.expected %t.result/comment.expected
// RUN: not %sourcekitd-test -req=syntactic-rename -rename-spec %S/syntactic-rename/invalid.in.json %s
// RUN: %sourcekitd-test -req=syntactic-rename -rename-spec %S/syntactic-rename/rename-memberwise.in.json %s >> %t.result/rename-memberwise.expected
// RUN: diff -u %S/syntactic-rename/rename-memberwise.expected %t.result/rename-memberwise.expected
// RUN: %sourcekitd-test -req=syntactic-rename -rename-spec %S/syntactic-rename/rename-layer.in.json %s >> %t.result/rename-layer.expected
// RUN: diff -u %S/syntactic-rename/rename-layer.expected %t.result/rename-layer.expected
// RUN: %sourcekitd-test -req=syntactic-rename -rename-spec %S/syntactic-rename/rename-P.in.json %s -- -swift-version 3 >> %t.result/rename-P.expected
// RUN: diff -u %S/syntactic-rename/rename-P.expected %t.result/rename-P.expected
// RUN: %sourcekitd-test -req=syntactic-rename -rename-spec %S/syntactic-rename/keywordbase.in.json %s -- -swift-version 3 >> %t.result/keywordbase.expected
// RUN: diff -u %S/syntactic-rename/keywordbase.expected %t.result/keywordbase.expected

// RUN: %empty-directory(%t.ranges)
// RUN: %sourcekitd-test -req=find-rename-ranges -rename-spec %S/syntactic-rename/x.in.json %s >> %t.ranges/x.expected
// RUN: diff -u %S/find-rename-ranges/x.expected %t.ranges/x.expected
// RUN: %sourcekitd-test -req=find-rename-ranges -rename-spec %S/syntactic-rename/z.in.json %s >> %t.ranges/z.expected
// RUN: diff -u %S/find-rename-ranges/z.expected %t.ranges/z.expected
// RUN: %sourcekitd-test -req=find-rename-ranges -rename-spec %S/syntactic-rename/foo.in.json %s >> %t.ranges/foo_arity1.expected
// RUN: diff -u %S/find-rename-ranges/foo_arity1.expected %t.ranges/foo_arity1.expected
// RUN: %sourcekitd-test -req=find-rename-ranges -rename-spec %S/syntactic-rename/bar.in.json %s >> %t.ranges/bar.expected
// RUN: diff -u %S/find-rename-ranges/bar.expected %t.ranges/bar.expected
// RUN: %sourcekitd-test -req=find-rename-ranges -rename-spec %S/syntactic-rename/comment.in.json %s >> %t.ranges/comment.expected
// RUN: diff -u %S/find-rename-ranges/comment.expected %t.ranges/comment.expected
// RUN: %sourcekitd-test -req=find-rename-ranges -rename-spec %S/syntactic-rename/init.in.json %s >> %t.ranges/init.expected
// RUN: diff -u %S/find-rename-ranges/init.expected %t.ranges/init.expected
// RUN: %sourcekitd-test -req=find-rename-ranges -rename-spec %S/syntactic-rename/enum_case.in.json %s >> %t.result/enum_case.expected
// RUN: diff -u %S/syntactic-rename/enum_case.expected %t.result/enum_case.expected
// RUN: %sourcekitd-test -req=find-rename-ranges -rename-spec %S/syntactic-rename/rename-memberwise.in.json %s >> %t.ranges/rename-memberwise.expected
// RUN: diff -u %S/find-rename-ranges/rename-memberwise.expected %t.ranges/rename-memberwise.expected
// RUN: %sourcekitd-test -req=find-rename-ranges -rename-spec %S/syntactic-rename/rename-layer.in.json %s >> %t.ranges/rename-layer.expected
// RUN: diff -u %S/find-rename-ranges/rename-layer.expected %t.ranges/rename-layer.expected
// RUN: %sourcekitd-test -req=find-rename-ranges -rename-spec %S/syntactic-rename/rename-P.in.json %s -- -swift-version 3 >> %t.ranges/rename-P.expected
// RUN: diff -u %S/find-rename-ranges/rename-P.expected %t.ranges/rename-P.expected
// RUN: %sourcekitd-test -req=find-rename-ranges -rename-spec %S/syntactic-rename/keywordbase.in.json %s -- -swift-version 3 >> %t.ranges/keywordbase.expected
// RUN: diff -u %S/find-rename-ranges/keywordbase.expected %t.ranges/keywordbase.expected
