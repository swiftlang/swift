// RUN: %swift-ide-test -annotate -source-filename %s | FileCheck %s

// CHECK: struct <Struct>S</Struct> {
// CHECK:   var <Var>x</Var> : <TypeAlias@>Int</TypeAlias>
// CHECK:   var <Var>y</Var> : swift.<TypeAlias@>Int</TypeAlias>
// CHECK: }
struct S {
  var x : Int
  var y : swift.Int
}

// CHECK: class <Class>MyCls</Class> {
// CHECK:   var <Var>www</Var> : <TypeAlias@>Int</TypeAlias>
// CHECK:   func <Func>foo</Func>(x : <TypeAlias@>Int</TypeAlias>) {}
// CHECK: }
class MyCls {
  var www : Int
  func foo(x : Int) {}
}

// CHECK: func <Func>foo</Func>(n : <TypeAlias@>Float</TypeAlias>) -> <TypeAlias@>Int</TypeAlias> {
// CHECK:   var <Var>q</Var> = MyCls<Constructor@[[@LINE-6]]:7>()
// CHECK:  </Constructor> var <Var>ee</Var> = "yoo";
// CHECK:   return 100009
// CHECK: }
func foo(n : Float) -> Int {
  var q = MyCls()
  var ee = "yoo";
  return 100009
}

// CHECK: protocol <Protocol>Prot</Protocol> {
// CHECK:   typealias <AssociatedType>Blarg</AssociatedType>
// CHECK:   func <Func>protMeth</Func>(x: <TypeAlias@>Int</TypeAlias>)
// CHECK: }
protocol Prot {
  typealias Blarg
  func protMeth(x: Int)
}
// CHECK: protocol <Protocol>Prot2</Protocol> {}
protocol Prot2 {}

// CHECK: class <Class>SubCls</Class> : <Class@[[@LINE-27]]:7>MyCls</Class>, <Protocol@[[@LINE-7]]:10>Prot</Protocol> {
// CHECK:   typealias <TypeAlias>Blarg</TypeAlias> = Prot2
// CHECK:   func <Func>protMeth</Func>(x: <TypeAlias@>Int</TypeAlias>) {}
// CHECK: }
class SubCls : MyCls, Prot {
  typealias Blarg = Prot2
  func protMeth(x: Int) {}
}

// CHECK: func <Func>genFn</Func><T : Prot where T.Blarg : Prot2>(p : T) -> <TypeAlias@>Int</TypeAlias> {}
func genFn<T : Prot where T.Blarg : Prot2>(p : T) -> Int {}

// FIXME: Constructors.
func test(x: Int) {
  genFn(SubCls())
  "This is string \(genFn({(a:Int) in SubCls()}(x))) interpolation"
}

func bar(x: Int) -> (Int, Float) {
  foo(Float())
}
