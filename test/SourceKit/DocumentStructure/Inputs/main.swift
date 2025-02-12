class Foo : Bar {
    var test : Int
    @IBOutlet var testOutlet : Int

    func testMethod() {
        if test {
        }
    }

    @IBAction func testAction() {
    }
}

@IBDesignable
class Foo2 {}

class Foo3 {
    @IBInspectable var testIBInspectable : Int
    @GKInspectable var testGKInspectable : Int
}

protocol MyProt {}

class OuterCls {
    class InnerCls1 {}
}

extension OuterCls {
    class InnerCls2 {}
}

class GenCls<T1, T2> {}

class TestParamAndCall {
    func testParams(arg1: Int, name: String) {
        if (arg1) {
            testParams(0, name:"testing")
        }
    }

    func testParamAndArg(arg1: Int, param par: Int) {
    }
}

// FIXME: Whatever.

class TestMarkers {
    // TODO: Something.
    func test(arg1: Bool) -> Int {
        // FIXME: Blah.
        if (arg1) {
            // FIXME: Blah.
            return 0
        }
        return 1
    }
}

func test2(arg1: Bool) {
    if (arg1) {
        // http://whatever FIXME: http://whatever/fixme.
    }
}

extension Foo {
    func anExtendedFooFunction() {
    }
}

// rdar://19539259 
var (sd2: Qtys)
{
  417(d: nil)
}

for i in 0...5 {}
for var i = 0, i2 = 1; i == 0; ++i {}
while var v = o, var z = o, v > z {}
repeat {} while v == 0
if var v = o, var z = o, v > z {}
switch v {
  case 1: break;
  case 2, 3: break;
  default: break;
}

let myArray = [1, 2, 3]
let myDict = [1:1, 2:2, 3:3]

// rdar://21203366
@objc
class ClassObjcAttr : NSObject {
    @objc
    func m() {}
}

@objc(Blah)
class ClassObjcAttr2 : NSObject {
    @objc(Foo)
    func m() {}
}

protocol FooProtocol {
    associatedtype Bar
    associatedtype Baz: Equatable
}

// https://github.com/apple/swift/issues/48287
a.b(c: d?.e?.f, h: i)

// https://github.com/apple/swift/issues/49474
/* 👨‍👩‍👧‍👦👨‍👩‍👧‍👦👨‍👩‍👧‍👦 */
`init`(x: Int, y: Int) {}
class C {
/* 👨‍👩‍👧‍👦👨‍👩‍👧‍👦👨‍👩‍👧‍👦 */
`init`(x: Int, y: Int) {}
}
var // comment
  `$` = 1
func /* comment */`foo`(x: Int) {}

// rdar://40085232
enum MyEnum {
  case Bar(arg: Int)
}

enum MySecondEnum {
  case One = 1
}

func someFunc(input :Int?, completion: () throws -> Void) rethrows {}

class OneMore {
  @IBSegueAction func testAction(coder: AnyObject, _ ident: String) -> AnyObject {
    fatalError()
  }
}

class Chain<A> {
  func + (lhs: Chain<A>, rhs: Chain<A>) -> Chain<A> { fatalError() }
}

public init() {
    fatalError()
}

deinit {
    fatalError()
}

#if false
extension Result {
  func foo() {}
}

extension Outer {
  class Inner {
    deinit {}
  }
}

public extension Outer2 {
  class Inner2 {
    deinit {}
  }
}
#endif

@objc(FPBarProto)
protocol BarProtocol {}

var var_with_didset = 10 {
  didSet { print(oldValue) }
}

#if os(iOS)
@objc protocol MyProtocol: NSObjectProtocol {
    var thing: NSObject {get}
}
#endif

class A {
  #if true
  @IBAction @objc func foo(a: Int) {}
  #endif
}

func testPostfixIfConfig() {
  foo
  #if FLAG1
    .flag1
    #if FLAG2
      .flag2
    #elseif FLAG3
      .flag3
    #else
      .else1
    #endif
    .flag1Tail
  #else
    .else2
  #endif
}
