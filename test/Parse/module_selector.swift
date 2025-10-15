// RUN: %target-typecheck-verify-swift -sdk %clang-importer-sdk -module-name main -I %S/Inputs -enable-experimental-feature ModuleSelector -parse

// Make sure the lack of the experimental flag disables the feature:
// RUN: not %target-typecheck-verify-swift -sdk %clang-importer-sdk -module-name main -I %S/Inputs 2>/dev/null

// REQUIRES: swift_feature_ModuleSelector

// ModuleSelectorCorrectCode
extension ModuleSelectorTestingKit::A {}

extension A: @retroactive Swift::Equatable {
  @_implements(Swift::Equatable, ==(_:_:))
  public static func equals(_: ModuleSelectorTestingKit::A, _: ModuleSelectorTestingKit::A) -> Swift::Bool {
    Swift::fatalError()
  }

  // FIXME: Add tests with autodiff @_differentiable(jvp:vjp:) and
  // @_derivative(of:)

  @_dynamicReplacement(for: ModuleSelectorTestingKit::negate())
  mutating func myNegate() {
    let fn: (Swift::Int, Swift::Int) -> Swift::Int = (Swift::+)

    let magnitude: Int.Swift::Magnitude = main::magnitude

    if Swift::Bool.Swift::random() {
      self.ModuleSelectorTestingKit::negate()
    } else {
      self = ModuleSelectorTestingKit::A(value: .Swift::min)
      self = A.ModuleSelectorTestingKit::init(value: .min)
    }

    self.main::myNegate()

    Swift::fatalError()
  }

  // FIXME: Can we test @convention(witness_method:)?
}


extension ModuleSelectorIncorrectAttrNames {
  // An attribute with a module selector *must* be a custom attribute and should be parsed as such.
  @main::available(macOS 10.15, *) var use1: String { "foo" }
  // expected-error@-1 {{expected ',' separator}}

  @main::available var use2

  @main::available(foo: bar) var use3

  func builderUser2(@main::MyBuilder fn: () -> Void) {}
}

// Repeat this test case at top level to make sure we correctly skip attribute
// module selectors when looking ahead to distinguish declarations from
// expressions.
@main::available(macOS 10.15, *) var use1: String { "foo" }
// expected-error@-1 {{expected ',' separator}}


// ModuleSelectorWhitespace
_ = Swift::print

_ = Swift:: print

_ = Swift ::print

_ = Swift :: print

_ = Swift::
print
// expected-error@-2 {{expected identifier after module selector}}
// expected-note@-3 {{remove extraneous whitespace after '::'}} {{12-+1:1=}}

_ = Swift
::print

_ = Swift ::
print
// expected-error@-2 {{expected identifier after module selector}}
// expected-note@-3 {{remove extraneous whitespace after '::'}} {{13-+1:1=}}

_ = Swift
:: print

_ = Swift
::
print
// expected-error@-2 {{expected identifier after module selector}}
// expected-note@-3 {{remove extraneous whitespace after '::'}} {{3-+1:1=}}


// ModuleSelectorIncorrectFuncSignature
func main::decl1() {}
// expected-error@-1 {{expected '(' in argument list of function declaration}} {{none}}
// expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{10-10=;}}
// expected-error@-3 {{expected module name in module selector}} {{none}}

func decl1(
  main::p1: Swift::A
// expected-error@-2 {{expected parameter name followed by ':'}} {{none}}
) {}

// Round-tripping failures:
func decl1(
  main::p1: ::A
// expected-error@-2 {{expected parameter name followed by ':'}} {{none}}
) {}

func decl1(
  main::p1: Swift::
// expected-error@-2 {{expected parameter name followed by ':'}} {{none}}
) {}

func decl1(
  main::label p2: Swift::inout A
// expected-error@-2 {{expected parameter name followed by ':'}} {{none}}
) {}

func decl1(
  label main::p3: @Swift::escaping () -> A
// expected-error@-1 {{expected parameter name followed by ':'}} {{none}}
// expected-error@-2 {{expected ',' separator}} {{13-13=,}}
// expected-error@-3 {{expected ':' following argument label and parameter name}} {{none}}
) {}


func testModuleSelectorIncorrectBindingDecls() {
  let main::decl1a = "a"
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}} {{11-11=;}}
  // expected-error@-2 {{expected module name in module selector}}

  // Found by mutation testing:
  let let::decl1a = "a"
  // expected-error@-1 {{'let' cannot appear nested inside another 'var' or 'let' pattern}}
  // expected-error@-2 {{expected pattern}}

  var main::decl1b = "b"
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}} {{11-11=;}}
  // expected-error@-2 {{expected module name in module selector}}

  let (main::decl1c, Swift::decl1d) = ("c", "d")
  // expected-error@-1 {{expected ',' separator}} {{12-12=,}}
  // expected-error@-2 {{expected pattern}}

  if let (main::decl1e, Swift::decl1f) = Optional(("e", "f")) {}
  // expected-error@-1 {{expected ',' separator}} {{15-15=,}}
  // expected-error@-2 {{expected module name in module selector}}

  guard let (main::decl1g, Swift::decl1h) = Optional(("g", "h")) else { return }
  // expected-error@-1 {{expected ',' separator}} {{18-18=,}}
  // expected-error@-2 {{expected module name in module selector}}

  switch Optional(main::decl1g) {
  case Optional.some(let Swift::decl1i):
    // expected-error@-1 {{expected ',' separator}} {{31-31=,}}
    // expected-error@-2 {{expected module name in module selector}}
    break
  case .none:
    break
  }

  switch Optional(main::decl1g) {
  case let Optional.some(Swift::decl1j):
    // expected-error@-1 {{expected ',' separator}} {{31-31=,}}
    // expected-error@-2 {{expected module name in module selector}}
    break
  case .none:
    break
  }

  switch Optional(main::decl1g) {
  case let Swift::decl1k?:
    // expected-error@-1 {{expected ':' after 'case'}} {{none}}
    // expected-error@-2 {{expected module name in module selector}}
    // expected-error@-3 {{consecutive statements on a line must be separated by ';'}} {{26-26=;}}
    // expected-error@-4 {{expected expression}}
    break
  case .none:
    break
  }

  for main::decl1l in "lll" {}
  // expected-error@-1 {{expected 'in' after for-each pattern}} {{none}}
  // expected-error@-2 {{expected module name in module selector}} {{none}}
  // expected-error@-3 {{expected '{' to start the body of for-each loop}}
}


// ModuleSelectorIncorrectClosureDecls
// This gets radically misinterpreted as two statements followed by some invalid code.
"lll".forEach { [Swift::magnitude]
  main::elem in print(elem)
  // expected-error@-1 {{expected expression}}
  // expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{13-13=;}}
}

"lll".forEach { (main::elem) in print(elem) }
// expected-error@-1 {{expected parameter name followed by ':'}} {{none}}
// expected-error@-2 {{expected ',' separator}} {{22-22=,}}

"lll".forEach { (main::elem) -> Void in print(elem) }
// expected-error@-1 {{expected parameter name followed by ':'}} {{none}}
// expected-error@-2 {{expected ',' separator}} {{22-22=,}}

"lll".forEach { (main::elem: Character) -> Void in print(elem) }
// expected-error@-1 {{expected parameter name followed by ':'}}
// expected-error@-2 {{expected ',' separator}} {{22-22=,}}


// ModuleSelectorIncorrectTypeDecls
enum main::decl2 {}
// expected-error@-1 {{expected '{' in enum}}
enum decl2_substitute {
  // expected-note@-1 {{in declaration of 'decl2_substitute'}}
  case Swift::decl2a
  // expected-error@-1 {{expected declaration}}
  // expected-error@-2 {{consecutive declarations on a line must be separated by ';'}} {{13-13=;}}
}

struct main::decl3 {}
// expected-error@-1 {{expected '{' in struct}}

class main::decl4<Swift::T> {}
// expected-error@-1 {{expected '{' in class}}

typealias main::decl5 = Swift::Bool
// expected-error@-1 {{expected '=' in type alias declaration}}

protocol main::decl6 {}
// expected-error@-1 {{expected '{' in protocol type}}
protocol decl6_substitute {
  // expected-note@-1 {{in declaration of 'decl6_substitute'}}
  associatedtype Swift::decl6a
  // expected-error@-1 {{expected declaration}}
  // expected-error@-2 {{consecutive declarations on a line must be separated by ';'}} {{23-23=;}}
}


// ModuleSelectorIncorrectGlobalVarDecls
let main::decl7 = 7
// expected-error@-1 {{consecutive statements on a line must be separated by ';'}} {{9-9=;}}
// expected-error@-2 {{expected module name in module selector}}

var main::decl8 = 8 {
// expected-error@-1 {{consecutive statements on a line must be separated by ';'}} {{9-9=;}}
// expected-error@-2 {{expected module name in module selector}}
// expected-error@-3 {{consecutive statements on a line must be separated by ';'}} {{20-20=;}}
// expected-error@-4 {{top-level statement cannot begin with a closure expression}}
  willSet(Swift::newValue) {}
  didSet(Foo::oldValue) {}
}

var decl8_willSet = 8 {
  willSet(Swift::newValue) {}
// expected-error@-1 {{expected ')' after willSet parameter name}}
// expected-note@-2 {{to match this opening '('}}
// expected-error@-3 {{expected '{' to start 'willSet' definition}}
  didSet(Foo::oldValue) {}
}

var decl8_didSet = 8 {
  willSet(newValue) {}
  didSet(Foo::oldValue) {}
  // expected-error@-1 {{expected ')' after didSet parameter name}}
  // expected-note@-2 {{to match this opening '('}}
  // expected-error@-3 {{expected '{' to start 'didSet' definition}}
}


// ModuleSelectorIncorrectNestedDecls
struct Parent {
  // expected-note@-1 {{in declaration of 'Parent'}}

  func main::decl1() {}
  // expected-error@-1 {{expected '(' in argument list of function declaration}}
  // expected-error@-2 {{consecutive declarations on a line must be separated by ';'}} {{12-12=;}}
  // expected-error@-3 {{expected declaration}}

  enum main::decl2 {}
  // expected-error@-1 {{expected '{' in enum}}

  enum decl2_substitute {
    // expected-note@-1 {{in declaration of 'decl2_substitute'}}
    case Swift::decl2a
    // expected-error@-1 {{consecutive declarations on a line must be separated by ';'}} {{15-15=;}}
    // expected-error@-2 {{expected declaration}}
  }

  struct main::decl3 {}
// expected-error@-1 {{expected '{' in struct}}

  class main::decl4 {}
// expected-error@-1 {{expected '{' in class}}

  typealias main::decl5 = Swift::Bool
// expected-error@-1 {{expected '=' in type alias declaration}}
}


// ModuleSelectorMacroDecls
struct CreatesDeclExpectation {
  #main::myMacro()
}


// ModuleSelectorIncorrectRuntimeBaseAttr
@_swift_native_objc_runtime_base(main::BaseClass)
// expected-error@-1 {{expected ')' in '_swift_native_objc_runtime_base' attribute}}
// expected-error@-2 {{expected declaration}}
class C1 {}


// ModuleSelectorOperatorDecls
infix operator <<<<< : Swift::AdditionPrecedence
// expected-error@-1 {{consecutive statements on a line must be separated by ';'}} {{29-29=;}}
// expected-error@-2 {{expected module name in module selector}}

precedencegroup main::PG1 {}
// expected-error@-1 {{expected '{' after name of precedence group}}
precedencegroup PG1_substitute {
  higherThan: Swift::AdditionPrecedence
// expected-error@-1 {{expected operator attribute identifier in precedence group body}}
}


// ModuleSelectorIllFormedModuleNames
var a: ::Int
// expected-error@-1 {{expected module name in module selector}}

var b: (::Int)
// expected-error@-1 {{expected module name in module selector}}

var c: *::Int
// expected-error@-1 {{expected type}}
// expected-error@-2 {{consecutive statements on a line must be separated by ';'}}
// expected-error@-3 {{expected module name in module selector}}

var d: _::Int
// expected-error@-1 {{'_' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{8-9=`_`}}

var e: Self::Int
// expected-error@-1 {{keyword 'Self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{8-12=`Self`}}

var f: self::Int
// expected-error@-1 {{keyword 'self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{8-12=`self`}}

var g: inout::Int
// expected-error@-1 {{expected module name in module selector}}

var h: Any::Int
// expected-error@-1 {{keyword 'Any' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{8-11=`Any`}}

var aArray: [::Int]
// expected-error@-1 {{expected module name in module selector}}

var bArray: [(::Int)]
// expected-error@-1 {{expected module name in module selector}}

var cArray: [*::Int]
// expected-error@-1 {{expected element type}}
// expected-error@-2 {{expected ']' in array type}}
// expected-note@-3 {{to match this opening '['}}
// expected-error@-4 {{consecutive statements on a line must be separated by ';'}} {{14-14=;}}
// expected-error@-5 {{expected module name in module selector}}
// expected-error@-6 {{consecutive statements on a line must be separated by ';'}} {{20-20=;}}
// expected-error@-7 {{expected expression}}

var dArray: [_::Int]
// expected-error@-1 {{'_' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{14-15=`_`}}

var eArray: [Self::Int]
// expected-error@-1 {{keyword 'Self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{14-18=`Self`}}

var fArray: [self::Int]
// expected-error@-1 {{keyword 'self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{14-18=`self`}}

var gArray: [inout::Int]
// expected-error@-1 {{expected module name in module selector}}

var hArray: [Any::Int]
// expected-error@-1 {{keyword 'Any' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{14-17=`Any`}}

var aIndex: String.::Index
// expected-error@-1 {{expected module name in module selector}}

// FIXME: This gets interpreted as a single `.*` operator; may not be ideal.
var cIndex: String.*::Index
// expected-error@-1 {{consecutive statements on a line must be separated by ';'}} {{19-19=;}}
// expected-error@-2 {{expected expression after unary operator}}
// expected-error@-3 {{expected module name in module selector}}

var dIndex: String._::Index
// expected-error@-1 {{'_' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{20-21=`_`}}

var eIndex: String.Self::Index
// expected-error@-1 {{keyword 'Self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{20-24=`Self`}}

var fIndex: String.self::Index
// expected-error@-1 {{keyword 'self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{20-24=`self`}}

var gIndex: String.inout::Index
// expected-error@-1 {{expected identifier in dotted type}}
// expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{20-20=;}}
// expected-error@-3 {{expected expression}}

var hIndex: String.Any::Index
// expected-error@-1 {{keyword 'Any' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{20-23=`Any`}}

func inExpr() {
  ::print()
// expected-error@-1 {{expected module name in module selector}}
}

func inExpr() {
  (::print())
// expected-error@-1 {{expected module name in module selector}}
}

func inExpr() {
  *::print()
// expected-error@-1 {{expected module name in module selector}}
}

func inExpr() {
  _::print()
// expected-error@-1 {{'_' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{3-4=`_`}}
}

func inExpr() {
  Self::print()
// expected-error@-1 {{keyword 'Self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{3-7=`Self`}}
}

func inExpr() {
  self::print()
// expected-error@-1 {{keyword 'self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{3-7=`self`}}
}

func inExpr() {
  inout::print()
// expected-error@-1 {{expected expression}}
}

func inExpr() {
  Any::print()
// expected-error@-1 {{keyword 'Any' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{3-6=`Any`}}
}

func inExpr() {
  _ = 1.::magnitude
// expected-error@-1 {{expected module name in module selector}}
}

func inExpr() {
  _ = (1.::magnitude)
// expected-error@-1 {{expected module name in module selector}}
}

// FIXME: This gets interpreted as a single `.*` operator; may not be ideal.
func inExpr() {
  _ = 1.*::magnitude
// expected-error@-1 {{expected module name in module selector}}
}

func inExpr() {
  _ = 1._::magnitude
// expected-error@-1 {{'_' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{9-10=`_`}}
}

func inExpr() {
  _ = 1.Self::magnitude
// expected-error@-1 {{keyword 'Self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{9-13=`Self`}}
}

func inExpr() {
  _ = 1.self::magnitude
// expected-error@-1 {{keyword 'self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{9-13=`self`}}
}

func inExpr() {
  _ = 1.inout::magnitude
// expected-error@-1 {{consecutive statements on a line must be separated by ';'}} {{14-14=;}}
// expected-error@-2 {{expected module name in module selector}}
}

func inExpr() {
  _ = 1.Any::magnitude
// expected-error@-1 {{keyword 'Any' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{9-12=`Any`}}
}


// ModuleSelectorAttrs
@_spi(main::Private)
// expected-error@-1 {{expected ')' in '_spi' attribute}} {{none}}
// expected-error@-2 {{expected declaration}}
public struct BadImplementsAttr: CustomStringConvertible {}

@_implements(main::CustomStringConvertible, Swift::description)
// expected-error@-1 {{expected ')' in '_implements' attribute}}
// expected-note@-2 {{to match this opening '('}}
// expected-error@-3 {{expected declaration}}
public var stringValue: String { fatalError() }

@_specialize(target: main::fn(), spi: Swift::Private, where T == Swift::Int)
// expected-error@-1 {{missing ',' in '_specialize' attribute}} {{none}}
// expected-error@-2 {{missing ',' in '_specialize' attribute}} {{none}}
public func fn<T>() -> T { fatalError() }

func fn(_: @isolated(Swift::any) () -> Void) {} 
// expected-error@-1 {{expected 'any' as the isolation kind}}
// expected-error@-2 {{expected ')' after isolation kind}} {{none}}
// expected-note@-3 {{to match this opening '('}}
// expected-error@-4 {{expected module name in module selector}}
// expected-error@-5 {{consecutive statements on a line must be separated by ';'}} {{44-44=;}}
// expected-error@-6 {{expected expression}}
// expected-error@-7 {{cannot have more than one parameter list}} FIXME: wat?

@_documentation(metadata: Swift::GroupName)
// expected-error@-1 {{expected ',' separator}} {{32-32=,}}
// expected-error@-2 {{'_documentation' attribute expected 'visibility' or 'metadata' argument}}
func fn() {}

@derivative(of: Swift::Foo.Swift::Bar.Swift::baz(), wrt: quux)
func fn() {}


// ModuleSelectorExpr
let x = Swift::do { 1 }

let x = Swift::
do { 1 }
// expected-error@-2 {{expected identifier after module selector}}
// expected-note@-3 {{remove extraneous whitespace after '::'}} {{16-+1:1=}}

let x = Swift::if y { 1 } else { 0 }
// expected-error@-1 {{consecutive statements on a line must be separated by ';'}} {{18-18=;}}
// expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{26-26=;}}
// expected-error@-3 {{expected expression}}

let x = Swift::
if y { 1 } else { 0 }
// expected-error@-2 {{expected identifier after module selector}}
// expected-note@-3 {{remove extraneous whitespace after '::'}} {{16-+1:1=}}

let x = Swift::switch y {
// expected-error@-1 {{consecutive statements on a line must be separated by ';'}} {{22-22=;}}
// expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{24-24=;}}
  // expected-error@-3 {{top-level statement cannot begin with a closure expression}}
case true: 1
// expected-error@-1 {{'case' label can only appear inside a 'switch' statement}}
case false: 0
// expected-error@-1 {{'case' label can only appear inside a 'switch' statement}}
}

let x = Swift::
// expected-error@-1 {{expected identifier after module selector}}
// expected-note@-2 {{remove extraneous whitespace after '::'}} {{16-+3:1=}}
switch y {
case true: 1
case false: 0
}

fn(Swift::&x)
// expected-error@-1 {{expected identifier after module selector}}
// expected-error@-2 {{expected ',' separator}}

_ = Swift::\main::Foo.BarKit::bar
// expected-error@-1 {{expected identifier after module selector}}
// expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{12-12=;}}

_ = \main::Foo.BarKit::bar

_ = Swift::-x
// expected-error@-1 {{expected identifier after module selector}}
// expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{12-12=;}}

_ = Swift::1
// expected-error@-1 {{expected identifier after module selector}}
// expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{12-12=;}}

_ = Swift::1.0
// expected-error@-1 {{expected identifier after module selector}}
// expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{12-12=;}}

func fn() {
  _ = Swift::@"fnord"
  // expected-error@-1 {{expected identifier after module selector}}
  // expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{14-14=;}}
  // expected-error@-3 {{string literals in Swift are not preceded by an '@' sign}}
}

_ = Swift::"fnord"
// expected-error@-1 {{expected identifier after module selector}}
// expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{12-12=;}}

_ = Swift::/fnord/
// expected-error@-1 {{expected identifier after module selector}}
// expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{12-12=;}}

_ = Swift::nil

_ = Swift::true

_ = Swift::identifier

_ = Swift::self

func fn() {
  // FIXME: ASTGen might be doing something weird here
  _ = Swift::init
}

@attached(extension, names: Swift::deinit) macro m()
// expected-error@-1 {{unknown introduced name kind 'Swift'}}
// expected-error@-2 {{expected '{' for deinitializer}}

@attached(extension, names: Swift::subscript) macro m()
// expected-error@-1 {{unknown introduced name kind 'Swift'}}
// expected-error@-2 {{expected '(' for subscript parameters}}

_ = Swift::Self

_ = Swift::Any

_ = {
  _ = Swift::$0
  // expected-error@-1 {{expected identifier after module selector}}
  // expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{14-14=;}}
}

_ = Swift::$foo

_ = Swift::_

Swift::_ = 1

_ = Swift::#foo
// expected-error@-1 {{consecutive statements on a line must be separated by ';'}} {{12-12=;}}
// expected-error@-2 {{expected identifier after module selector}}

_ = #Swift::foo

_ = Swift::{ 1 }
// expected-error@-1 {{expected identifier after module selector}}

_ = Swift::.random()
// expected-error@-1 {{expected identifier after module selector}}

_ = Swift::.main::random()
// expected-error@-1 {{expected identifier after module selector}}

_ = .main::random()

_ = Swift::super.foo()

_ = Swift::(a, b)
// expected-error@-1 {{expected identifier after module selector}}

_ = Swift::[a, b]
// expected-error@-1 {{expected identifier after module selector}}

_ = Swift::
// expected-error@-1 {{expected identifier after module selector}}
// expected-note@-2 {{remove extraneous whitespace after '::'}} {{12-+4:1=}}

_ = x.Swift::y

_ = x.Swift::1
// expected-error@-1 {{expected identifier after module selector}}

_ = x.Swift::self

_ = x.Swift::Self.self

_ = x.Swift::Type.self

_ = x.Swift::Protocol.self

_ = myArray.reduce(0, Swift::+)

if Swift::#available(macOS 15, *) {}
// expected-error@-1 {{expected identifier after module selector}}
// expected-error@-2 {{expected '{' after 'if' condition}}

func fn(_: Swift::Self) {}

func fn(_: Swift::Any) {}

func fn(_: Swift::Foo) {}

func fn(_: Swift::(Int, String)) {}
// expected-error@-1 {{expected identifier after module selector}}

func fn(_: Swift::[Int]) {}
// expected-error@-1 {{expected identifier after module selector}}

func fn(_: Swift::_) {}

func fn(_: Swift::) {}
// expected-error@-1 {{expected identifier after module selector}}

func fn(_: Foo.Swift::Type) {}

func fn(_: Foo.Swift::Protocol) {}

func fn(_: Foo.Swift::Bar) {}

func fn(_: Foo.Swift::self) {}


// ModuleSelectorSubmodule

_ = Foundation::NSData::NSData()
// expected-error@-1 {{module selector cannot specify a submodule}} {{17-25=}}

_ = Foundation::NSData::Fnord::NSData()
// expected-error@-1 {{module selector cannot specify a submodule}} {{17-32=}}

_ = Foundation::NSData::
Fnord::NSData()
// expected-error@-2 {{module selector cannot specify a submodule}} {{17-25=}}
// expected-error@-3 {{expected identifier after module selector}}
// expected-note@-4 {{remove extraneous whitespace after '::'}} {{25-+1:1=}}
