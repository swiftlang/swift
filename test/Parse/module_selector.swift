// RUN: %target-typecheck-verify-swift -sdk %clang-importer-sdk -module-name main -I %S/Inputs -parse -verify-additional-prefix legacy-
// RUN: %target-typecheck-verify-swift -sdk %clang-importer-sdk -module-name main -I %S/Inputs -parse -verify-additional-prefix new- -enable-experimental-feature ParserASTGen

// Make sure the lack of the experimental flag disables the feature:
// RUN: not %target-typecheck-verify-swift -sdk %clang-importer-sdk -module-name main -I %S/Inputs 2>/dev/null

// REQUIRES: swift_feature_ParserASTGen

// ModuleSelectorImports
import struct ModuleSelectorTestingKit::A

import struct _::A
// expected-error@-1 {{'_' cannot be used as an identifier here}}
// expected-legacy-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{15-16=`_`}}

import struct ModuleSelectorTestingKit::Submodule::A
// expected-legacy-error@-1 {{module selector cannot specify a submodule}} {{41-52=}} expected-new-error@-1 {{unexpected code 'Submodule::' in import}}

import struct ModuleSelectorTestingKit.Submodule::A
// expected-legacy-error@-1 {{module selector cannot specify a submodule}} expected-new-error@-1 {{submodule cannot be imported using module selector}}
// expected-legacy-note@-2 {{replace '::' with '.'}} {{49-51=.}} expected-new-note@-2 {{replace '::' with '.'}} {{49-51=}} {{49-49=.}}

import ctypes::bits
// expected-legacy-error@-1 {{module selector cannot specify a submodule}} expected-new-error@-1 {{submodule cannot be imported using module selector}}
// expected-legacy-note@-2 {{replace '::' with '.'}} {{14-16=.}} expected-new-note@-2 {{replace '::' with '.'}} {{14-16=}} {{14-14=.}}


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
  // expected-legacy-error@-1 {{expected ',' separator}} expected-new-error@-1 {{unexpected code '10.15, *' in attribute}}

  @main::available var use2

  @main::available(foo: bar) var use3

  func builderUser2(@main::MyBuilder fn: () -> Void) {}
}

// Repeat this test case at top level to make sure we correctly skip attribute
// module selectors when looking ahead to distinguish declarations from
// expressions.
@main::available(macOS 10.15, *) var use1: String { "foo" }
// expected-legacy-error@-1 {{expected ',' separator}} expected-new-error@-1 {{unexpected code '10.15, *' in attribute}}


// ModuleSelectorWhitespace
_ = Swift::print

_ = Swift:: print

_ = Swift ::print

_ = Swift :: print

_ = Swift::
print
// expected-legacy-error@-2 {{expected identifier after module selector}} expected-new-error@-2 {{expected identifier}}
// expected-legacy-note@-3 {{remove extraneous whitespace after '::'}} {{12-+1:1=}} expected-new-note@-3 {{insert identifier}} {{12-12=<#identifier#>}}

_ = Swift
::print

_ = Swift ::
print
// expected-legacy-error@-2 {{expected identifier after module selector}} expected-new-error@-2 {{expected identifier}}
// expected-legacy-note@-3 {{remove extraneous whitespace after '::'}} {{13-+1:1=}} expected-new-note@-3 {{insert identifier}} {{13-13=<#identifier#>}}

_ = Swift
:: print

_ = Swift
::
print
// expected-legacy-error@-2 {{expected identifier after module selector}} expected-new-error@-2 {{expected identifier}}
// expected-legacy-note@-3 {{remove extraneous whitespace after '::'}} {{3-+1:1=}} expected-new-note@-3 {{insert identifier}} {{3-3=<#identifier#>}}


// ModuleSelectorIncorrectFuncSignature
func main::decl1() {}
// expected-legacy-error@-1 {{expected '(' in argument list of function declaration}} {{none}} expected-new-error@-1 {{unexpected code '::decl1' before parameter clause}}
// expected-legacy-error@-2 {{consecutive statements on a line must be separated by ';'}} {{10-10=;}}
// expected-legacy-error@-3 {{expected module name in module selector}} {{none}}

func decl1(
  main::p1: Swift::A
// expected-legacy-error@-2 {{expected parameter name followed by ':'}} {{none}} expected-new-error@-1 {{unexpected code '::p1' in parameter}}
) {}

// Round-tripping failures:
func decl1(
  main::p1: ::A
// expected-legacy-error@-2 {{expected parameter name followed by ':'}} {{none}} expected-new-error@-1 {{unexpected code '::p1' in parameter}}
// expected-new-error@-2 {{expected module name in module selector}}
// expected-new-note@-3 {{insert module name}} {{13-13=<#identifier#>}}
) {}

func decl1(
  main::p1: Swift::
// expected-legacy-error@-2 {{expected parameter name followed by ':'}} {{none}} expected-new-error@-1 {{unexpected code '::p1' in parameter}}
// expected-new-error@-2 {{expected identifier in type}}
// expected-new-note@-3 {{insert identifier}} {{20-20=<#identifier#>}}
) {}

func decl1(
  main::label p2: Swift::inout A
// expected-legacy-error@-2 {{expected parameter name followed by ':'}} {{none}} expected-new-error@-1 {{unexpected code '::label p2' in parameter}}
// expected-new-error@-2 {{expected ',' in parameter}}
// expected-new-note@-3 {{insert ','}} {{31-32=}} {{32-32=, }}
// expected-new-error@-4 {{expected identifier and ':' in parameter}}
// expected-new-note@-5 {{insert identifier and ':'}} {{32-32=<#identifier#>}}
) {}

func decl1(
  label main::p3: @Swift::escaping () -> A
// expected-legacy-error@-1 {{expected parameter name followed by ':'}} {{none}} expected-new-error@-1 {{unexpected code '::p3' in parameter}}
// expected-legacy-error@-2 {{expected ',' separator}} {{13-13=,}}
// expected-legacy-error@-3 {{expected ':' following argument label and parameter name}} {{none}}
) {}


func testModuleSelectorIncorrectBindingDecls() {
  let main::decl1a = "a"
  // expected-new-error@-1 {{expected '=' in variable}}
  // expected-legacy-error@-2 {{consecutive statements on a line must be separated by ';'}} {{11-11=;}} expected-new-note@-2 {{insert '='}} {{11-11= = }}
  // expected-error@-3 {{expected module name in module selector}}
  // expected-new-note@-4 {{insert module name}} {{11-11=<#identifier#>}}

  // Found by mutation testing:
  let let::decl1a = "a"
  // expected-legacy-error@-1 {{'let' cannot appear nested inside another 'var' or 'let' pattern}} expected-new-error@-1 {{expected pattern in value binding pattern}}
  // expected-legacy-error@-2 {{expected pattern}} expected-new-note@-2 {{insert pattern}} {{10-10=<#pattern#> }}
  // expected-new-error@-3 {{expected '=' in variable}}
  // expected-new-note@-4 {{insert '='}} {{10-10== }}
  // expected-new-error@-5 {{expected module name in module selector}}
  // expected-new-note@-6 {{insert module name}} {{10-10=<#identifier#>}}

  var main::decl1b = "b"
  // expected-new-error@-1 {{expected '=' in variable}}
  // expected-legacy-error@-2 {{consecutive statements on a line must be separated by ';'}} {{11-11=;}} expected-new-note@-2 {{insert '='}} {{11-11= = }}
  // expected-error@-3 {{expected module name in module selector}}
  // expected-new-note@-4 {{insert module name}} {{11-11=<#identifier#>}}

  let (main::decl1c, Swift::decl1d) = ("c", "d")
  // expected-legacy-error@-1 {{expected ',' separator}} {{12-12=,}} expected-new-error@-1 {{unexpected code '::decl1c, Swift::decl1d' in tuple pattern}}
  // expected-legacy-error@-2 {{expected pattern}}

  if let (main::decl1e, Swift::decl1f) = Optional(("e", "f")) {}
  // expected-legacy-error@-1 {{expected ',' separator}} {{15-15=,}} expected-new-error@-1 {{unexpected code '::decl1e, Swift::decl1f' in tuple}}
  // expected-legacy-error@-2 {{expected module name in module selector}}

  guard let (main::decl1g, Swift::decl1h) = Optional(("g", "h")) else { return }
  // expected-legacy-error@-1 {{expected ',' separator}} {{18-18=,}} expected-new-error@-1 {{unexpected code '::decl1g, Swift::decl1h' in tuple}}
  // expected-legacy-error@-2 {{expected module name in module selector}}

  switch Optional(main::decl1g) {
  case Optional.some(let Swift::decl1i):
    // expected-legacy-error@-1 {{expected ',' separator}} {{31-31=,}} expected-new-error@-1 {{unexpected code '::decl1i' in function call}}
    // expected-legacy-error@-2 {{expected module name in module selector}}
    break
  case .none:
    break
  }

  switch Optional(main::decl1g) {
  case let Optional.some(Swift::decl1j):
    // expected-legacy-error@-1 {{expected ',' separator}} {{31-31=,}} expected-new-error@-1 {{unexpected code '::decl1j' in function call}}
    // expected-legacy-error@-2 {{expected module name in module selector}}
    break
  case .none:
    break
  }

  switch Optional(main::decl1g) {
  case let Swift::decl1k?:
    // expected-legacy-error@-1 {{expected ':' after 'case'}} {{none}} expected-new-error@-1 {{unexpected code '::decl1k?' in switch case}}
    // expected-legacy-error@-2 {{expected module name in module selector}}
    // expected-legacy-error@-3 {{consecutive statements on a line must be separated by ';'}} {{26-26=;}}
    // expected-legacy-error@-4 {{expected expression}}
    break
  case .none:
    break
  }

  for main::decl1l in "lll" {}
  // expected-legacy-error@-1 {{expected 'in' after for-each pattern}} {{none}} expected-new-error@-1 {{unexpected code '::decl1l' in 'for' statement}}
  // expected-legacy-error@-2 {{expected module name in module selector}} {{none}}
  // expected-legacy-error@-3 {{expected '{' to start the body of for-each loop}}
}


// ModuleSelectorIncorrectClosureDecls
// This gets radically misinterpreted as two statements followed by some invalid code.
"lll".forEach { [Swift::magnitude]
  main::elem in print(elem)
  // expected-legacy-error@-1 {{expected expression}} expected-new-error@-1 {{unexpected code 'in print(elem)' in closure}}
  // expected-legacy-error@-2 {{consecutive statements on a line must be separated by ';'}} {{13-13=;}}
}

"lll".forEach { (main::elem) in print(elem) }
// expected-legacy-error@-1 {{expected parameter name followed by ':'}} {{none}} expected-new-error@-1 {{unexpected code '::elem' in parameter clause}}
// expected-legacy-error@-2 {{expected ',' separator}} {{22-22=,}}

"lll".forEach { (main::elem) -> Void in print(elem) }
// expected-legacy-error@-1 {{expected parameter name followed by ':'}} {{none}} expected-new-error@-1 {{unexpected code '::elem' in parameter clause}}
// expected-legacy-error@-2 {{expected ',' separator}} {{22-22=,}}

"lll".forEach { (main::elem: Character) -> Void in print(elem) }
// expected-legacy-error@-1 {{expected parameter name followed by ':'}} expected-new-error@-1 {{unexpected code '::elem: Character' in parameter clause}}
// expected-legacy-error@-2 {{expected ',' separator}} {{22-22=,}}


// ModuleSelectorIncorrectTypeDecls
enum main::decl2 {}
// expected-legacy-error@-1 {{expected '{' in enum}} expected-new-error@-1 {{unexpected code '::decl2' in enum}}
enum decl2_substitute {
  // expected-legacy-note@-1 {{in declaration of 'decl2_substitute'}}
  case Swift::decl2a
  // expected-legacy-error@-1 {{expected declaration}} expected-new-error@-1 {{unexpected code '::decl2a' in enum}}
  // expected-legacy-error@-2 {{consecutive declarations on a line must be separated by ';'}} {{13-13=;}}
}

struct main::decl3 {}
// expected-legacy-error@-1 {{expected '{' in struct}} expected-new-error@-1 {{unexpected code '::decl3' in struct}}

class main::decl4<Swift::T> {}
// expected-legacy-error@-1 {{expected '{' in class}} expected-new-error@-1 {{unexpected code '::decl4<Swift::T>' in class}}

typealias main::decl5 = Swift::Bool
// expected-legacy-error@-1 {{expected '=' in type alias declaration}} expected-new-error@-1 {{unexpected code '::decl5' in typealias declaration}}

protocol main::decl6 {}
// expected-legacy-error@-1 {{expected '{' in protocol type}} expected-new-error@-1 {{unexpected code '::decl6' in protocol}}
protocol decl6_substitute {
  // expected-legacy-note@-1 {{in declaration of 'decl6_substitute'}}
  associatedtype Swift::decl6a
  // expected-legacy-error@-1 {{expected declaration}} expected-new-error@-1 {{unexpected code '::decl6a' in protocol}}
  // expected-legacy-error@-2 {{consecutive declarations on a line must be separated by ';'}} {{23-23=;}}
}


// ModuleSelectorIncorrectGlobalVarDecls
let main::decl7 = 7
// expected-legacy-error@-1 {{consecutive statements on a line must be separated by ';'}} {{9-9=;}} expected-new-error@-1 {{expected '=' in variable}}
// expected-new-note@-2 {{insert '='}} {{9-9= = }}
// expected-error@-3 {{expected module name in module selector}}
// expected-new-note@-4 {{insert module name}} {{9-9=<#identifier#>}}

var main::decl8 = 8 {
// expected-legacy-error@-1 {{consecutive statements on a line must be separated by ';'}} {{9-9=;}} expected-new-error@-1 {{expected '=' in variable}}
// expected-new-note@-2 {{insert '='}} {{9-9= = }}
// expected-error@-3 {{expected module name in module selector}}
// expected-new-note@-4 {{insert module name}} {{9-9=<#identifier#>}}
// expected-legacy-error@-5 {{consecutive statements on a line must be separated by ';'}} {{20-20=;}}
// expected-legacy-error@-6 {{top-level statement cannot begin with a closure expression}}
  willSet(Swift::newValue) {}
  // expected-new-error@-1 {{unexpected code '::newValue' in accessor}}
  didSet(Foo::oldValue) {}
  // expected-new-error@-1 {{unexpected code '::oldValue' in accessor}}
}

var decl8_willSet = 8 {
  willSet(Swift::newValue) {}
// expected-legacy-error@-1 {{expected ')' after willSet parameter name}} expected-new-error@-1 {{unexpected code '::newValue' in accessor}}
// expected-legacy-note@-2 {{to match this opening '('}}
// expected-legacy-error@-3 {{expected '{' to start 'willSet' definition}}
  didSet(Foo::oldValue) {}
// expected-new-error@-1 {{unexpected code '::oldValue' in accessor}}
}

var decl8_didSet = 8 {
  willSet(newValue) {}
  didSet(Foo::oldValue) {}
  // expected-legacy-error@-1 {{expected ')' after didSet parameter name}} expected-new-error@-1 {{unexpected code '::oldValue' in accessor}}
  // expected-legacy-note@-2 {{to match this opening '('}}
  // expected-legacy-error@-3 {{expected '{' to start 'didSet' definition}}
}


// ModuleSelectorIncorrectNestedDecls
struct Parent {
  // expected-legacy-note@-1 {{in declaration of 'Parent'}}

  func main::decl1() {}
  // expected-legacy-error@-1 {{expected '(' in argument list of function declaration}} expected-new-error@-1 {{unexpected code '::decl1' before parameter clause}}
  // expected-legacy-error@-2 {{consecutive declarations on a line must be separated by ';'}} {{12-12=;}}
  // expected-legacy-error@-3 {{expected declaration}}

  enum main::decl2 {}
  // expected-legacy-error@-1 {{expected '{' in enum}} expected-new-error@-1 {{unexpected code '::decl2' in enum}}

  enum decl2_substitute {
    // expected-legacy-note@-1 {{in declaration of 'decl2_substitute'}}
    case Swift::decl2a
    // expected-legacy-error@-1 {{consecutive declarations on a line must be separated by ';'}} {{15-15=;}}  expected-new-error@-1 {{unexpected code '::decl2a' in enum}}
    // expected-legacy-error@-2 {{expected declaration}}
  }

  struct main::decl3 {}
// expected-legacy-error@-1 {{expected '{' in struct}} expected-new-error@-1 {{unexpected code '::decl3' in struct}}

  class main::decl4 {}
// expected-legacy-error@-1 {{expected '{' in class}} expected-new-error@-1 {{unexpected code '::decl4' in class}}

  typealias main::decl5 = Swift::Bool
// expected-legacy-error@-1 {{expected '=' in type alias declaration}} expected-new-error@-1 {{unexpected code '::decl5' in typealias declaration}}
}


// ModuleSelectorMacroDecls
struct CreatesDeclExpectation {
  #main::myMacro()
}


// ModuleSelectorIncorrectRuntimeBaseAttr
@_swift_native_objc_runtime_base(main::BaseClass)
// expected-legacy-error@-1 {{expected ')' in '_swift_native_objc_runtime_base' attribute}} FIXME: Should be diagnosed in ASTGen
// expected-legacy-error@-2 {{expected declaration}}
class C1 {}


// ModuleSelectorOperatorDecls
infix operator <<<<< : Swift::AdditionPrecedence
// expected-legacy-error@-1 {{consecutive statements on a line must be separated by ';'}} {{29-29=;}} expected-new-error@-1 {{consecutive statements on a line must be separated by newline or ';'}}
// expected-new-note@-2 {{insert newline}} {{29-29=\n}}
// expected-new-note@-3 {{insert ';'}} {{29-29=;}}
// expected-error@-4 {{expected module name in module selector}}
// expected-new-note@-5 {{insert module name}} {{29-29=<#identifier#>}}

precedencegroup main::PG1 {}
// expected-legacy-error@-1 {{expected '{' after name of precedence group}} expected-new-error@-1 {{unexpected code '::PG1' in precedencegroup}}
precedencegroup PG1_substitute {
  higherThan: Swift::AdditionPrecedence
// expected-legacy-error@-1 {{expected operator attribute identifier in precedence group body}} expected-new-error@-1 {{unexpected code '::AdditionPrecedence' in precedencegroup}}
}


// ModuleSelectorIllFormedModuleNames
var a: ::Int
// expected-error@-1 {{expected module name in module selector}}
// expected-new-note@-2 {{insert module name}} {{8-8=<#identifier#>}}

var b: (::Int)
// expected-error@-1 {{expected module name in module selector}}
// expected-new-note@-2 {{insert module name}} {{9-9=<#identifier#>}}

var c: *::Int
// expected-legacy-error@-1 {{expected type}} expected-new-error@-1 {{expected type in type annotation}}
// expected-new-note@-2 {{insert type}} {{8-8=<#type#>}}
// expected-legacy-error@-3 {{consecutive statements on a line must be separated by ';'}} expected-new-error@-3 {{unexpected code '*::Int' in source file}}
// expected-legacy-error@-4 {{expected module name in module selector}}

var d: _::Int
// expected-error@-1 {{'_' cannot be used as an identifier here}}
// expected-legacy-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{8-9=`_`}}

var e: Self::Int
// expected-error@-1 {{keyword 'Self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{8-12=`Self`}}

var f: self::Int
// expected-error@-1 {{keyword 'self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{8-12=`self`}}

var g: inout::Int
// expected-new-error@-1 {{expected type in type annotation}}
// expected-new-note@-2 {{insert type}} {{8-8=<#type#>}}
// expected-new-error@-3 {{expected pattern in variable}}
// expected-new-note@-4 {{insert pattern}} {{13-13=<#pattern#> }}
// expected-new-error@-5 {{expected '=' in variable}}
// expected-new-note@-6 {{insert '='}} {{13-13== }}
// expected-legacy-error@-7 {{expected module name in module selector}} expected-new-error@-7 {{expected module name in module selector}}
// expected-new-note@-8 {{insert module name}} {{13-13=<#identifier#>}}

var h: Any::Int
// expected-error@-1 {{keyword 'Any' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{8-11=`Any`}}

var aArray: [::Int]
// expected-error@-1 {{expected module name in module selector}}
// expected-new-note@-2 {{insert module name}} {{14-14=<#identifier#>}}

var bArray: [(::Int)]
// expected-error@-1 {{expected module name in module selector}}
// expected-new-note@-2 {{insert module name}} {{15-15=<#identifier#>}}

var cArray: [*::Int]
// expected-legacy-error@-1 {{expected element type}} expected-new-error@-1 {{expected type in array type}}
// expected-new-note@-2 {{insert type}} {{14-14=<#type#>}}
// expected-legacy-error@-3 {{expected ']' in array type}}
// expected-legacy-note@-4 {{to match this opening '['}}
// expected-legacy-error@-5 {{consecutive statements on a line must be separated by ';'}} {{14-14=;}} expected-new-error@-5 {{unexpected code '*::Int' in array type}}
// expected-legacy-error@-6 {{expected module name in module selector}}
// expected-legacy-error@-7 {{consecutive statements on a line must be separated by ';'}} {{20-20=;}}
// expected-legacy-error@-8 {{expected expression}}

var dArray: [_::Int]
// expected-error@-1 {{'_' cannot be used as an identifier here}}
// expected-legacy-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{14-15=`_`}}

var eArray: [Self::Int]
// expected-error@-1 {{keyword 'Self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{14-18=`Self`}}

var fArray: [self::Int]
// expected-error@-1 {{keyword 'self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{14-18=`self`}}

var gArray: [inout::Int]
// expected-new-error@-1 {{expected type and ']' to end array type}}
// expected-new-note@-2 {{insert type and ']'}} {{14-14=<#type#>}} {{14-14=]}}
// expected-new-error@-3 {{expected pattern in variable}}
// expected-new-note@-4 {{insert pattern}} {{19-19=<#pattern#> }}
// expected-new-error@-5 {{expected '=' in variable}}
// expected-new-note@-6 {{insert '='}} {{19-19== }}
// expected-error@-7 {{expected module name in module selector}}
// expected-new-note@-8 {{insert module name}} {{19-19=<#identifier#>}}
// expected-new-error@-9 {{unexpected code ']' in source file}}

var hArray: [Any::Int]
// expected-error@-1 {{keyword 'Any' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{14-17=`Any`}}

var aIndex: String.::Index
// expected-error@-1 {{expected module name in module selector}}
// expected-new-note@-2 {{insert module name}} {{20-20=<#identifier#>}}

// FIXME: This gets interpreted as a single `.*` operator; may not be ideal.
var cIndex: String.*::Index
// expected-legacy-error@-1 {{consecutive statements on a line must be separated by ';'}} {{19-19=;}}
// expected-legacy-error@-2 {{expected expression after unary operator}} expected-new-error@-2 {{unexpected code '.*::Index' in source file}}
// expected-legacy-error@-3 {{expected module name in module selector}}

var dIndex: String._::Index
// expected-error@-1 {{'_' cannot be used as an identifier here}}
// expected-legacy-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{20-21=`_`}}

var eIndex: String.Self::Index
// expected-error@-1 {{keyword 'Self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{20-24=`Self`}}

var fIndex: String.self::Index
// expected-error@-1 {{keyword 'self' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{20-24=`self`}}

var gIndex: String.inout::Index
// expected-legacy-error@-1 {{expected identifier in dotted type}} expected-new-error@-1 {{expected '=' in variable}}
// expected-new-note@-2 {{insert '='}} {{25-25= = }}
// expected-legacy-error@-3 {{consecutive statements on a line must be separated by ';'}} {{20-20=;}}
// expected-legacy-error@-4 {{expected expression}} expected-new-error@-4 {{expected module name in module selector}}
// expected-new-note@-5 {{insert module name}} {{25-25=<#identifier#>}}

var hIndex: String.Any::Index
// expected-error@-1 {{keyword 'Any' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{20-23=`Any`}}

func inExpr() {
  ::print()
// expected-error@-1 {{expected module name in module selector}}
// expected-new-note@-2 {{insert module name}} {{-1:16-+0:3=}} {{-1:16-16=\n  <#identifier#>}}
}

func inExpr() {
  (::print())
// expected-error@-1 {{expected module name in module selector}}
// expected-new-note@-2 {{insert module name}} {{4-4=<#identifier#>}}
}

func inExpr() {
  *::print()
// expected-legacy-error@-1 {{expected module name in module selector}} expected-new-error@-1 {{unexpected code '*::print()' in function}}
}

func inExpr() {
  _::print()
// expected-error@-1 {{'_' cannot be used as an identifier here}}
// expected-legacy-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{3-4=`_`}}
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
// expected-legacy-error@-1 {{expected expression}} expected-new-error@-1 {{expected pattern in variable}}
// expected-new-note@-2 {{insert pattern}} {{8-8=<#pattern#> }}
// expected-new-error@-3 {{expected '=' in variable}}
// expected-new-note@-4 {{insert '='}} {{8-8== }}
// expected-new-error@-5 {{expected module name in module selector}}
// expected-new-note@-6 {{insert module name}} {{8-8=<#identifier#>}}
}

func inExpr() {
  Any::print()
// expected-error@-1 {{keyword 'Any' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{3-6=`Any`}}
}

func inExpr() {
  _ = 1.::magnitude
// expected-error@-1 {{expected module name in module selector}}
// expected-new-note@-2 {{insert module name}} {{9-9=<#identifier#>}}
}

func inExpr() {
  _ = (1.::magnitude)
// expected-error@-1 {{expected module name in module selector}}
// expected-new-note@-2 {{insert module name}} {{10-10=<#identifier#>}}
}

// FIXME: This gets interpreted as a single `.*` operator; may not be ideal.
func inExpr() {
  _ = 1.*::magnitude
// expected-new-error@-1 {{consecutive statements on a line must be separated by newline or ';'}}
// expected-new-note@-2 {{insert newline}} {{10-10=\n  }}
// expected-new-note@-3 {{insert ';'}} {{10-10=;}}
// expected-error@-4 {{expected module name in module selector}}
// expected-new-note@-5 {{insert module name}} {{10-10=<#identifier#>}}
}

func inExpr() {
  _ = 1._::magnitude
// expected-error@-1 {{'_' cannot be used as an identifier here}}
// expected-legacy-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{9-10=`_`}}
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
// expected-legacy-error@-1 {{consecutive statements on a line must be separated by ';'}} {{14-14=;}} expected-new-error@-1 {{consecutive statements on a line must be separated by newline or ';'}}
// expected-new-note@-2 {{insert newline}} {{14-14=\n  }}
// expected-new-note@-3 {{insert ';'}} {{14-14=;}}
// expected-error@-4 {{expected module name in module selector}}
// expected-new-note@-5 {{insert module name}} {{14-14=<#identifier#>}}
}

func inExpr() {
  _ = 1.Any::magnitude
// expected-error@-1 {{keyword 'Any' cannot be used as an identifier here}}
// expected-note@-2 {{if this name is unavoidable, use backticks to escape it}} {{9-12=`Any`}}
}


// ModuleSelectorAttrs
@_spi(main::Private)
// expected-legacy-error@-1 {{expected ')' in '_spi' attribute}} {{none}} FIXME: 'main::Private' should be diagnosed in ASTGen
// expected-legacy-error@-2 {{expected declaration}}
public struct BadImplementsAttr: CustomStringConvertible {}

@_implements(main::CustomStringConvertible, Swift::description)
// expected-legacy-error@-1 {{expected ')' in '_implements' attribute}} FIXME: 'Swift::description' should be diagnosed in ASTGen
// expected-legacy-note@-2 {{to match this opening '('}}
// expected-legacy-error@-3 {{expected declaration}}
public var stringValue: String { fatalError() }

@_specialize(target: main::fn(), spi: Swift::Private, where T == Swift::Int)
// expected-legacy-error@-1 {{missing ',' in '_specialize' attribute}} {{none}} FIXME: 'main::fn()' should be diagnosed in ASTGen
// expected-legacy-error@-2 {{missing ',' in '_specialize' attribute}} {{none}} expected-new-error@-2 {{unexpected code '::Private, where T == Swift::Int' in attribute}}
public func fn<T>() -> T { fatalError() }

func fn(_: @isolated(Swift::any) () -> Void) {} 
// expected-legacy-error@-1 {{expected 'any' as the isolation kind}} expected-new-error@-1 {{unexpected code '::any' in attribute}}
// expected-legacy-error@-2 {{expected ')' after isolation kind}} {{none}}
// expected-legacy-note@-3 {{to match this opening '('}}
// expected-legacy-error@-4 {{expected module name in module selector}}
// expected-legacy-error@-5 {{consecutive statements on a line must be separated by ';'}} {{44-44=;}}
// expected-legacy-error@-6 {{expected expression}}
// expected-legacy-error@-7 {{cannot have more than one parameter list}} FIXME: wat?

@_documentation(metadata: Swift::GroupName)
// expected-legacy-error@-1 {{expected ',' separator}} {{32-32=,}} expected-new-error@-1 {{unexpected code '::GroupName' in attribute}}
// expected-legacy-error@-2 {{'_documentation' attribute expected 'visibility' or 'metadata' argument}}
func fn() {}

@derivative(of: Swift::Foo.Swift::Bar.Swift::baz(), wrt: quux)
func fn() {}


// ModuleSelectorExpr
let x = Swift::do { 1 }

let x = Swift::
do { 1 }
// expected-legacy-error@-2 {{expected identifier after module selector}} expected-new-error@-2 {{expected identifier in variable}}
// expected-legacy-note@-3 {{remove extraneous whitespace after '::'}} {{16-+1:1=}} expected-new-note@-3 {{insert identifier}} {{16-16=<#identifier#>}}

let x = Swift::if y { 1 } else { 0 }
// expected-legacy-error@-1 {{consecutive statements on a line must be separated by ';'}} {{18-18=;}} expected-new-error@-1 {{consecutive statements on a line must be separated by newline or ';'}}
// expected-new-note@-2 {{insert newline}} {{18-19= \n}}
// expected-new-note@-3 {{insert ';'}} {{18-19=}} {{19-19=; }}
// expected-legacy-error@-4 {{consecutive statements on a line must be separated by ';'}} {{26-26=;}}
// expected-legacy-error@-5 {{expected expression}} expected-new-error@-5 {{unexpected code 'else { 0 }' in source file}}

let x = Swift::
if y { 1 } else { 0 }
// expected-legacy-error@-2 {{expected identifier after module selector}} expected-new-error@-2 {{expected identifier in variable}}
// expected-legacy-note@-3 {{remove extraneous whitespace after '::'}} {{16-+1:1=}} expected-new-note@-3 {{insert identifier}} {{16-16=<#identifier#>}}

let x = Swift::switch y {
// expected-legacy-error@-1 {{consecutive statements on a line must be separated by ';'}} {{22-22=;}} expected-new-error@-1 {{consecutive statements on a line must be separated by newline or ';'}}
// expected-new-note@-2 {{insert newline}} {{22-23= \n}}
// expected-new-note@-3 {{insert ';'}} {{22-23=}} {{23-23=; }}
// expected-legacy-error@-4 {{consecutive statements on a line must be separated by ';'}} {{24-24=;}} expected-new-error@-4 {{consecutive statements on a line must be separated by newline or ';'}}
// expected-new-note@-5 {{insert newline}} {{24-25= \n}}
// expected-new-note@-6 {{insert ';'}} {{24-25=}} {{25-25=; }}
  // expected-legacy-error@-7 {{top-level statement cannot begin with a closure expression}}
case true: 1
// expected-legacy-error@-1 {{'case' label can only appear inside a 'switch' statement}} expected-new-error@-1 {{'case' can only appear inside a 'switch' statement or 'enum' declaration}}
case false: 0
// expected-legacy-error@-1 {{'case' label can only appear inside a 'switch' statement}} expected-new-error@-1 {{'case' can only appear inside a 'switch' statement or 'enum' declaration}}
}

let x = Swift::
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier in variable}}
// expected-legacy-note@-2 {{remove extraneous whitespace after '::'}} {{16-+3:1=}} expected-new-note@-2 {{insert identifier}} {{16-16=<#identifier#>}}
switch y {
case true: 1
case false: 0
}

fn(Swift::&x)
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier in function call}}
// expected-new-note@-2 {{insert identifier}} {{11-11=<#identifier#>}}
// expected-legacy-error@-3 {{expected ',' separator}} expected-new-error@-3 {{unexpected code '&x' in function call}}

_ = Swift::\main::Foo.BarKit::bar
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier}}
// expected-new-note@-2 {{insert identifier}} {{12-12=<#identifier#>}}
// expected-legacy-error@-3 {{consecutive statements on a line must be separated by ';'}} {{12-12=;}}

_ = \main::Foo.BarKit::bar

_ = Swift::-x
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier}}
// expected-new-note@-2 {{insert identifier}} {{12-12=<#identifier#>}}
// expected-legacy-error@-3 {{consecutive statements on a line must be separated by ';'}} {{12-12=;}}

_ = Swift::1
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier}}
// expected-new-note@-2 {{insert identifier}} {{12-12=<#identifier#>}}
// expected-legacy-error@-3 {{consecutive statements on a line must be separated by ';'}} {{12-12=;}}

_ = Swift::1.0
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier}}
// expected-new-note@-2 {{insert identifier}} {{12-12=<#identifier#>}}
// expected-legacy-error@-3 {{consecutive statements on a line must be separated by ';'}} {{12-12=;}}

func fn() {
  _ = Swift::@"fnord"
  // expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier}}
  // expected-new-note@-2 {{insert identifier}} {{14-14=<#identifier#>}}
  // expected-legacy-error@-3 {{consecutive statements on a line must be separated by ';'}} {{14-14=;}}
  // expected-error@-4 {{string literals in Swift are not preceded by an '@' sign}}
  // expected-new-note@-5 {{remove '@'}} {{14-15=}}
}

_ = Swift::"fnord"
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier}}
// expected-new-note@-2 {{insert identifier}} {{12-12=<#identifier#>}}
// expected-legacy-error@-3 {{consecutive statements on a line must be separated by ';'}} {{12-12=;}}

_ = Swift::/fnord/
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier}}
// expected-new-note@-2 {{insert identifier}} {{12-12=<#identifier#>}}
// expected-legacy-error@-3 {{consecutive statements on a line must be separated by ';'}} {{12-12=;}}

_ = Swift::nil

_ = Swift::true

_ = Swift::identifier

_ = Swift::self

func fn() {
  // FIXME: ASTGen might be doing something weird here
  _ = Swift::init
}

@attached(extension, names: Swift::deinit) macro m()
// expected-legacy-error@-1 {{unknown introduced name kind 'Swift'}} FIXME: 'Swift::' should be diagnosed by ASTGen
// expected-legacy-error@-2 {{expected '{' for deinitializer}}

@attached(extension, names: Swift::subscript) macro m()
// expected-legacy-error@-1 {{unknown introduced name kind 'Swift'}} FIXME: 'Swift::' should be diagnosed by ASTGen
// expected-legacy-error@-2 {{expected '(' for subscript parameters}}

_ = Swift::Self

_ = Swift::Any

_ = {
  _ = Swift::$0
  // expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier}}
  // expected-new-note@-2 {{insert identifier}} {{14-14=<#identifier#>}}
  // expected-legacy-error@-3 {{consecutive statements on a line must be separated by ';'}} {{14-14=;}}
}

_ = Swift::$foo

//  FIXME: Legacy parser considers `_` a keyword; new parser probably should too
_ = Swift::_
// expected-new-error@-1 {{expected identifier}}
// expected-new-note@-2 {{insert identifier}} {{12-12=<#identifier#>}}

Swift::_ = 1
// expected-new-error@-1 {{expected identifier}}
// expected-new-note@-2 {{insert identifier}} {{8-8=<#identifier#>}}

_ = Swift::#foo
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier}}
// expected-new-note@-2 {{insert identifier}} {{12-12=<#identifier#>}}
// expected-legacy-error@-3 {{consecutive statements on a line must be separated by ';'}}

_ = #Swift::foo

_ = Swift::{ 1 }
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier in function call}}
// expected-new-note@-2 {{insert identifier}} {{12-12=<#identifier#> }}

_ = Swift::.random()
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier in member access}}
// expected-new-note@-2 {{insert identifier}} {{12-12=<#identifier#>}}

_ = Swift::.main::random()
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier in member access}}
// expected-new-note@-2 {{insert identifier}} {{12-12=<#identifier#>}}

_ = .main::random()

_ = Swift::super.foo()

_ = Swift::(a, b)
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier in function call}}
// expected-new-note@-2 {{insert identifier}} {{12-12=<#identifier#>}}

_ = Swift::[a, b]
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier in subscript}}
// expected-new-note@-2 {{insert identifier}} {{12-12=<#identifier#>}}

_ = Swift::
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier}}
// expected-legacy-note@-2 {{remove extraneous whitespace after '::'}} {{12-+4:1=}} expected-new-note@-2 {{insert identifier}} {{12-12=<#identifier#>}}

_ = x.Swift::y

_ = x.Swift::1
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier in member access}}
// expected-new-note@-2 {{insert identifier}} {{14-14=<#identifier#>}}

_ = x.Swift::self

_ = x.Swift::Self.self

_ = x.Swift::Type.self

_ = x.Swift::Protocol.self

_ = myArray.reduce(0, Swift::+)

if Swift::#available(macOS 15, *) {}
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier in 'if' statement}}
// expected-new-note@-2 {{insert identifier}} {{11-11=<#identifier#> }}
// expected-legacy-error@-3 {{expected '{' after 'if' condition}} expected-new-error@-3 {{unexpected code '#available(macOS 15, *)' in 'if' statement}}

func fn(_: Swift::Self) {}

func fn(_: Swift::Any) {}

func fn(_: Swift::Foo) {}

func fn(_: Swift::(Int, String)) {}
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier in type}}
// expected-new-note@-2 {{insert identifier}} {{19-19=<#identifier#>}}
// expected-new-error@-3 {{unexpected code '(Int, String)' in parameter clause}}

func fn(_: Swift::[Int]) {}
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier in type}}
// expected-new-note@-2 {{insert identifier}} {{19-19=<#identifier#>}}
// expected-new-error@-3 {{unexpected code '[Int]' in parameter clause}}

//  FIXME: Legacy parser considers `_` a keyword; new parser probably should too
func fn(_: Swift::_) {}
// expected-new-error@-1 {{expected identifier in type}}
// expected-new-note@-2 {{insert identifier}} {{19-19=<#identifier#>}}
// expected-new-error@-3 {{unexpected code '_' in parameter clause}}

func fn(_: Swift::) {}
// expected-legacy-error@-1 {{expected identifier after module selector}} expected-new-error@-1 {{expected identifier in type}}
// expected-new-note@-2 {{insert identifier}} {{19-19=<#identifier#>}}

func fn(_: Foo.Swift::Type) {}

func fn(_: Foo.Swift::Protocol) {}

func fn(_: Foo.Swift::Bar) {}

func fn(_: Foo.Swift::self) {}


// ModuleSelectorSubmodule

_ = Foundation::NSData::NSData()
// expected-legacy-error@-1 {{module selector cannot specify a submodule}} {{17-25=}} expected-new-error@-1 {{unexpected code 'NSData::' in module selector}}

_ = Foundation::NSData::Fnord::NSData()
// expected-legacy-error@-1 {{module selector cannot specify a submodule}} {{17-32=}} expected-new-error@-1 {{unexpected code 'NSData::Fnord::' in module selector}}

_ = Foundation::NSData::
Fnord::NSData()
// expected-legacy-error@-2 {{module selector cannot specify a submodule}} {{17-25=}} expected-new-error@-2 {{unexpected code 'NSData::' in module selector}}
// expected-legacy-error@-3 {{expected identifier after module selector}} expected-new-error@-3 {{expected identifier}}
// expected-legacy-note@-4 {{remove extraneous whitespace after '::'}} {{25-+1:1=}} expected-new-note@-4 {{insert identifier}} {{25-25=<#identifier#>}}
