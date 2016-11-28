// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

@objc
protocol OlfactoryProtocol {
  @available(*, unavailable)
  func bad() // expected-note {{here}}
  @available(*, unavailable, message: "it was smelly")
  func smelly() // expected-note {{here}}
  @available(*, unavailable, renamed: "new")
  func old() // expected-note {{here}}
  @available(*, unavailable, renamed: "new", message: "it was smelly")
  func oldAndSmelly() // expected-note {{here}}

  @available(*, unavailable)
  var badProp: Int { get } // expected-note {{here}}
  @available(*, unavailable, message: "it was smelly")
  var smellyProp: Int { get } // expected-note {{here}}
  @available(*, unavailable, renamed: "new")
  var oldProp: Int { get } // expected-note {{here}}
  @available(*, unavailable, renamed: "new", message: "it was smelly")
  var oldAndSmellyProp: Int { get } // expected-note {{here}}

  @available(*, unavailable, renamed: "init")
  func nowAnInitializer() // expected-note {{here}}
  @available(*, unavailable, renamed: "init()")
  func nowAnInitializer2() // expected-note {{here}}

  @available(*, unavailable, renamed: "foo")
  init(nowAFunction: Int) // expected-note {{here}}
  @available(*, unavailable, renamed: "foo(_:)")
  init(nowAFunction2: Int) // expected-note {{here}}

  @available(*, unavailable, renamed: "shinyLabeledArguments(example:)")
  func unavailableArgNames(a: Int) // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(example:)")
  func unavailableArgRenamed(a: Int) // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments()")
  func unavailableNoArgs() // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(a:)")
  func unavailableSame(a: Int) // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(example:)")
  func unavailableUnnamed(_ a: Int) // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(_:)")
  func unavailableUnnamedSame(_ a: Int) // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(_:)")
  func unavailableNewlyUnnamed(a: Int) // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(a:b:)")
  func unavailableMultiSame(a: Int, b: Int) // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(example:another:)")
  func unavailableMultiUnnamed(_ a: Int, _ b: Int) // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(_:_:)")
  func unavailableMultiUnnamedSame(_ a: Int, _ b: Int) // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(_:_:)")
  func unavailableMultiNewlyUnnamed(a: Int, b: Int) // expected-note {{here}}

  @available(*, unavailable, renamed: "init(shinyNewName:)")
  init(unavailableArgNames: Int) // expected-note{{here}}
  @available(*, unavailable, renamed: "init(a:)")
  init(_ unavailableUnnamed: Int) // expected-note{{here}}
  @available(*, unavailable, renamed: "init(_:)")
  init(unavailableNewlyUnnamed: Int) // expected-note{{here}}
  @available(*, unavailable, renamed: "init(a:b:)")
  init(_ unavailableMultiUnnamed: Int, _ b: Int) // expected-note{{here}}
  @available(*, unavailable, renamed: "init(_:_:)")
  init(unavailableMultiNewlyUnnamed a: Int, b: Int) // expected-note{{here}}

  @available(*, unavailable, renamed: "shinyLabeledArguments(x:)")
  func unavailableTooFew(a: Int, b: Int) // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(x:b:)")
  func unavailableTooMany(a: Int) // expected-note {{here}}
  @available(*, unavailable, renamed: "shinyLabeledArguments(x:)")
  func unavailableNoArgsTooMany() // expected-note {{here}}

  @available(*, unavailable, renamed: "Base.shinyLabeledArguments()")
  func unavailableHasType() // expected-note {{here}}
}

final class SchnozType : OlfactoryProtocol {
  @objc func bad() {} // expected-error {{cannot override 'bad' which has been marked unavailable}} {{none}}
  @objc func smelly() {} // expected-error {{cannot override 'smelly' which has been marked unavailable: it was smelly}} {{none}}
  @objc func old() {} // expected-error {{'old()' has been renamed to 'new'}} {{14-17=new}}
  @objc func oldAndSmelly() {} // expected-error {{'oldAndSmelly()' has been renamed to 'new': it was smelly}} {{14-26=new}}

  @objc var badProp: Int { return 0 } // expected-error {{cannot override 'badProp' which has been marked unavailable}} {{none}}
  @objc var smellyProp: Int { return 0 } // expected-error {{cannot override 'smellyProp' which has been marked unavailable: it was smelly}} {{none}}
  @objc var oldProp: Int { return 0 } // expected-error {{'oldProp' has been renamed to 'new'}} {{13-20=new}}
  @objc var oldAndSmellyProp: Int { return 0 } // expected-error {{'oldAndSmellyProp' has been renamed to 'new': it was smelly}} {{13-29=new}}

  @objc func nowAnInitializer() {} // expected-error {{'nowAnInitializer()' has been replaced by 'init'}} {{none}}
  @objc func nowAnInitializer2() {} // expected-error {{'nowAnInitializer2()' has been replaced by 'init()'}} {{none}}
  @objc init(nowAFunction: Int) {} // expected-error {{'init(nowAFunction:)' has been renamed to 'foo'}} {{none}}
  @objc init(nowAFunction2: Int) {} // expected-error {{'init(nowAFunction2:)' has been renamed to 'foo(_:)'}} {{none}}

  @objc func unavailableArgNames(a: Int) {} // expected-error {{'unavailableArgNames(a:)' has been renamed to 'shinyLabeledArguments(example:)'}} {{14-33=shinyLabeledArguments}} {{34-34=example }}
  @objc func unavailableArgRenamed(a param: Int) {} // expected-error {{'unavailableArgRenamed(a:)' has been renamed to 'shinyLabeledArguments(example:)'}} {{14-35=shinyLabeledArguments}} {{36-37=example}}
  @objc func unavailableNoArgs() {} // expected-error {{'unavailableNoArgs()' has been renamed to 'shinyLabeledArguments()'}} {{14-31=shinyLabeledArguments}}
  @objc func unavailableSame(a: Int) {} // expected-error {{'unavailableSame(a:)' has been renamed to 'shinyLabeledArguments(a:)'}} {{14-29=shinyLabeledArguments}}
  @objc func unavailableUnnamed(_ a: Int) {} // expected-error {{'unavailableUnnamed' has been renamed to 'shinyLabeledArguments(example:)'}} {{14-32=shinyLabeledArguments}} {{33-34=example}}
  @objc func unavailableUnnamedSame(_ a: Int) {} // expected-error {{'unavailableUnnamedSame' has been renamed to 'shinyLabeledArguments(_:)'}} {{14-36=shinyLabeledArguments}}
  @objc func unavailableNewlyUnnamed(a: Int) {} // expected-error {{'unavailableNewlyUnnamed(a:)' has been renamed to 'shinyLabeledArguments(_:)'}} {{14-37=shinyLabeledArguments}} {{38-38=_ }}
  @objc func unavailableMultiSame(a: Int, b: Int) {} // expected-error {{'unavailableMultiSame(a:b:)' has been renamed to 'shinyLabeledArguments(a:b:)'}} {{14-34=shinyLabeledArguments}}
  @objc func unavailableMultiUnnamed(_ a: Int, _ b: Int) {} // expected-error {{'unavailableMultiUnnamed' has been renamed to 'shinyLabeledArguments(example:another:)'}} {{14-37=shinyLabeledArguments}} {{38-39=example}} {{48-49=another}}
  @objc func unavailableMultiUnnamedSame(_ a: Int, _ b: Int) {} // expected-error {{'unavailableMultiUnnamedSame' has been renamed to 'shinyLabeledArguments(_:_:)'}} {{14-41=shinyLabeledArguments}}
  @objc func unavailableMultiNewlyUnnamed(a: Int, b: Int) {} // expected-error {{'unavailableMultiNewlyUnnamed(a:b:)' has been renamed to 'shinyLabeledArguments(_:_:)'}} {{14-42=shinyLabeledArguments}} {{43-43=_ }} {{51-51=_ }}

  @objc init(unavailableArgNames: Int) {} // expected-error {{'init(unavailableArgNames:)' has been renamed to 'init(shinyNewName:)'}} {{14-14=shinyNewName }}
  @objc init(_ unavailableUnnamed: Int) {} // expected-error {{'init' has been renamed to 'init(a:)'}} {{14-15=a}}
  @objc init(unavailableNewlyUnnamed: Int) {} // expected-error {{'init(unavailableNewlyUnnamed:)' has been renamed to 'init(_:)'}} {{14-14=_ }}
  @objc init(_ unavailableMultiUnnamed: Int, _ b: Int) {} // expected-error {{'init' has been renamed to 'init(a:b:)'}} {{14-15=a}} {{46-48=}}
  @objc init(unavailableMultiNewlyUnnamed a: Int, b: Int) {} // expected-error {{'init(unavailableMultiNewlyUnnamed:b:)' has been renamed to 'init(_:_:)'}} {{14-42=_}} {{51-51=_ }}

  @objc func unavailableTooFew(a: Int, b: Int) {} // expected-error {{'unavailableTooFew(a:b:)' has been renamed to 'shinyLabeledArguments(x:)'}} {{none}}
  @objc func unavailableTooMany(a: Int) {} // expected-error {{'unavailableTooMany(a:)' has been renamed to 'shinyLabeledArguments(x:b:)'}} {{none}}
  @objc func unavailableNoArgsTooMany() {} // expected-error {{'unavailableNoArgsTooMany()' has been renamed to 'shinyLabeledArguments(x:)'}} {{none}}
  @objc func unavailableHasType() {} // expected-error {{'unavailableHasType()' has been replaced by 'Base.shinyLabeledArguments()'}} {{none}}
}

// Make sure we can still conform to a protocol with unavailable requirements,
// and check for some bogus diagnostics not being emitted.
@objc
protocol Snout {
  @available(*, unavailable)
  func sniff()
}

class Tapir : Snout {}

class Elephant : Snout {
  @nonobjc func sniff() {}
}

class Puppy : Snout {}

extension Puppy {
  func sniff() {}
}

class Kitten : Snout {}

extension Kitten {
  @nonobjc func sniff() {}
}
