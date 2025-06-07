// RUN: %target-swift-frontend -print-ast -disable-objc-attr-requires-foundation-module -enable-objc-interop %s -diagnostic-style llvm 2>&1 | %FileCheck %s

var x = 0.0

type(of: x)
// CHECK: type(of: x)

class A {
  var x: Int = 3
  init() {
  }
}

class B: A {
  override init() {
    super.init()
    super.x = 2;
  }
}
// CHECK: @_inheritsConvenienceInitializers internal class B : A {
// CHECK:   override internal init() {
// CHECK:     super.init()
// CHECK:     super.x = 2
// CHECK:   }
// CHECK:   @objc deinit {
// CHECK:   }
// CHECK: }

var a: [Int] = [1, 3, 2];

for i in 0...2 {
  a[i] = i;
}
// CHECK: for i in 0 ... 2 {
// CHECK:   a[i] = i
// CHECK: }

let y: Double = 0 as Double
// CHECK: @_hasInitialValue internal let y: Double = 0 as Double

var stringToInt: Int? = Int("1");

var forceInt: Int = stringToInt!
// CHECK: @_hasInitialValue internal var forceInt: Int = stringToInt!

var prefixUnaryExpr: Int = -a[1];
// CHECK: @_hasInitialValue internal var prefixUnaryExpr: Int = -a[1]
var postfixUnaryExpr: Int = stringToInt!
// CHECK: @_hasInitialValue internal var postfixUnaryExpr: Int = stringToInt!

class MyID {
  var num = 1
}

class SomeJSON {
  var id: MyID?
}

let cnt = SomeJSON().id?.num
// CHECK: @_hasInitialValue internal let cnt: Int? = SomeJSON().id?.num

struct User {
  let name: String
  let email: String
  let address: Address?
  let role: Role
}

struct Address {
  let street: String
}

enum Role {
  case admin
  case member
  case guest
  
  var permissions: [Permission] {
    switch self {
    case .admin:
        return [.create, .read, .update, .delete]
    case .member:
        return [.create, .read]
    case .guest:
        return [.read]
    }
  }
}

enum Permission {
  case create
  case read
  case update
  case delete
}

var user = User(
  name: "Swift",
  email: "http://www.swift.org",
  address: nil,
  role: .admin
)


let userRoleKeyPath = \User.role
// CHECK: @_hasInitialValue internal let userRoleKeyPath: KeyPath<User, Role> = \User

let role = user[keyPath: userRoleKeyPath]
// CHECK: @_hasInitialValue internal let role: Role = user[keyPath: userRoleKeyPath]

struct FakeColor: _ExpressibleByColorLiteral {
  init(_colorLiteralRed: Float, green: Float, blue: Float, alpha: Float) {}
}
typealias _ColorLiteralType = FakeColor

let myColor = #colorLiteral(red: 0.292, green: 0.081, blue: 0.6, alpha: 255)
// CHECK: @_hasInitialValue internal let myColor: FakeColor = #colorLiteral(red: 0.292, green: 0.081, blue: 0.6, alpha: 255)

enum FileNames {
  static let main = FileNames.file(#file)
// CHECK: @_hasInitialValue internal static let main: FileNames = FileNames.file(#file)
  case file(String)
}

class C {
  @objc
  var foo = 0
}

func foo(_ x: AnyObject) {
  let y = x.foo
}
// CHECK: internal func foo(_ x: AnyObject) {
// CHECK:   @_hasInitialValue let y: Int? = x.foo
// CHECK: }

struct S {
  func foo() {}
}
let fn = S.foo
