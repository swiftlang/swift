// RUN: %target-typecheck-verify-swift -swift-version 4 -module-name main -package-name myPkg

public protocol P {
  associatedtype Element

  func f() -> Element
}

struct S<T> : P {
  func f() -> T { while true {} }
}

package struct PkgStruct<T> : P {
  package func f() -> T { while true {} }
}

public struct G<T> {
  typealias A = S<T> // expected-note {{type declared here}}
  package typealias B = PkgStruct<T> // expected-note {{type declared here}}

  public func foo<U : P>(arg: U) where U.Element == A.Element {}
  // expected-error@-1 {{instance method cannot be declared public because its generic requirement uses an internal type}}
  public func bar<U : P>(arg: U) where U.Element == B.Element {}
  // expected-error@-1 {{instance method cannot be declared public because its generic requirement uses a package type}}
}

public final class ReplayableGenerator<S: Sequence> : IteratorProtocol {
    typealias Sequence = S // expected-note {{type declared here}}
    public typealias Element = Sequence.Iterator.Element // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}

    package typealias PkgSequence = S // expected-note {{type declared here}}
    public typealias ElementPkg = PkgSequence.Iterator.Element // expected-error {{type alias cannot be declared public because its underlying type uses a package type}}

    public func next() -> Element? {
      return nil
    }
}

struct Generic<T> {
  fileprivate typealias Dependent = T // expected-note 2{{type declared here}}
}

package struct PkgGeneric<T> { // expected-note *{{type declared here}}
  package typealias Dependent = T // expected-note *{{type declared here}}
}

var x: Generic<Int>.Dependent = 3 // expected-error {{variable must be declared private or fileprivate because its type uses a fileprivate type}}

func internalFuncWithFileprivateAlias() -> Generic<Int>.Dependent { // expected-error {{function must be declared private or fileprivate because its result uses a fileprivate type}}
  return 3
}

private func privateFuncWithFileprivateAlias() -> Generic<Int>.Dependent {
  return 3
}

var y = privateFuncWithFileprivateAlias() // expected-error{{variable must be declared private or fileprivate because its type 'Generic<Int>.Dependent' (aka 'Int') uses a fileprivate type}}

public var z: PkgGeneric<Int>.Dependent = 3 // expected-error {{variable cannot be declared public because its type uses a package type}}

public func funcWithPackageAlias() -> PkgGeneric<Int>.Dependent { // expected-error {{function cannot be declared public because its result uses a package type}}
  return 3
}

package func pkgFuncWithPackageAlias() -> PkgGeneric<Int>.Dependent {
  return 3
}

public var zz = pkgFuncWithPackageAlias() // expected-error{{variable cannot be declared public because its type 'PkgGeneric<Int>.Dependent' (aka 'Int') uses a package type}}


private typealias FnType = (_ x: Int) -> Void // expected-note * {{type declared here}}
package typealias PkgFnType = (_ x: Int) -> Void // expected-note * {{type declared here}}

public var fn1: (FnType) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var fn2: (_ x: FnType) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var fn3: (main.FnType) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var fn4: (_ x: main.FnType) -> Void = { _ in } // expected-error {{cannot be declared public}}

public var fn1p: (PkgFnType) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var fn2p: (_ x: PkgFnType) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var fn3p: (main.PkgFnType) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var fn4p: (_ x: main.PkgFnType) -> Void = { _ in } // expected-error {{cannot be declared public}}

package var pfn1: (FnType) -> Void = { _ in } // expected-error {{cannot be declared package}}
package var pfn2: (_ x: FnType) -> Void = { _ in } // expected-error {{cannot be declared package}}
package var pfn3: (main.FnType) -> Void = { _ in } // expected-error {{cannot be declared package}}
package var pfn4: (_ x: main.FnType) -> Void = { _ in } // expected-error {{cannot be declared package}}

public var nested1: (_ x: (FnType) -> Void) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var nested2: (_ x: (main.FnType) -> Void) -> Void = { _ in } // expected-error {{cannot be declared public}}

public var nested1p: (_ x: (PkgFnType) -> Void) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var nested2p: (_ x: (main.PkgFnType) -> Void) -> Void = { _ in } // expected-error {{cannot be declared public}}

package var pnested1: (_ x: (FnType) -> Void) -> Void = { _ in } // expected-error {{cannot be declared package}}
package var pnested2: (_ x: (main.FnType) -> Void) -> Void = { _ in } // expected-error {{cannot be declared package}}

public func test1(x: FnType) {} // expected-error {{cannot be declared public}}
public func test2(x: main.FnType) {} // expected-error {{cannot be declared public}}

public func test1p(x: PkgFnType) {} // expected-error {{cannot be declared public}}
public func test2p(x: main.PkgFnType) {} // expected-error {{cannot be declared public}}

package func ptest1(x: FnType) {} // expected-error {{cannot be declared package}}
package func ptest2(x: main.FnType) {} // expected-error {{cannot be declared package}}

public func reject1(x: FnType?) {} // expected-error {{cannot be declared public}}
public func reject2(x: main.FnType?) {} // expected-error {{cannot be declared public}}
public func reject3() -> FnType { fatalError() } // expected-error {{cannot be declared public}}
public func reject4() -> main.FnType { fatalError() } // expected-error {{cannot be declared public}}

public func reject1p(x: PkgFnType?) {} // expected-error {{cannot be declared public}}
public func reject2p(x: main.PkgFnType?) {} // expected-error {{cannot be declared public}}
public func reject3p() -> PkgFnType { fatalError() } // expected-error {{cannot be declared public}}
public func reject4p() -> main.PkgFnType { fatalError() } // expected-error {{cannot be declared public}}

package func preject1(x: FnType?) {} // expected-error {{cannot be declared package}}
package func preject2(x: main.FnType?) {} // expected-error {{cannot be declared package}}
package func preject3() -> FnType { fatalError() } // expected-error {{cannot be declared package}}
package func preject4() -> main.FnType { fatalError() } // expected-error {{cannot be declared package}}

public var rejectVar1: FnType = {_ in } // expected-error {{cannot be declared public}}
public var rejectVar2: main.FnType = {_ in } // expected-error {{cannot be declared public}}
public var rejectVar3: FnType? // expected-error {{cannot be declared public}}
public var rejectVar4: main.FnType? // expected-error {{cannot be declared public}}

public var rejectVar1p: PkgFnType = {_ in } // expected-error {{cannot be declared public}}
public var rejectVar2p: main.PkgFnType = {_ in } // expected-error {{cannot be declared public}}
public var rejectVar3p: PkgFnType? // expected-error {{cannot be declared public}}
public var rejectVar4p: main.PkgFnType? // expected-error {{cannot be declared public}}

package var prejectVar1: FnType = {_ in } // expected-error {{cannot be declared package}}
package var prejectVar2: main.FnType = {_ in } // expected-error {{cannot be declared package}}
package var prejectVar3: FnType? // expected-error {{cannot be declared package}}
package var prejectVar4: main.FnType? // expected-error {{cannot be declared package}}

public var escaping1: (@escaping FnType) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var escaping2: (_ x: @escaping FnType) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var escaping3: (@escaping main.FnType) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var escaping4: (_ x: @escaping main.FnType) -> Void = { _ in } // expected-error {{cannot be declared public}}

public var escaping1p: (@escaping PkgFnType) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var escaping2p: (_ x: @escaping PkgFnType) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var escaping3p: (@escaping main.PkgFnType) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var escaping4p: (_ x: @escaping main.PkgFnType) -> Void = { _ in } // expected-error {{cannot be declared public}}

package var pescaping1: (@escaping FnType) -> Void = { _ in } // expected-error {{cannot be declared package}}
package var pescaping2: (_ x: @escaping FnType) -> Void = { _ in } // expected-error {{cannot be declared package}}
package var pescaping3: (@escaping main.FnType) -> Void = { _ in } // expected-error {{cannot be declared package}}
package var pescaping4: (_ x: @escaping main.FnType) -> Void = { _ in } // expected-error {{cannot be declared package}}

public struct SubscriptTest {
  public subscript(test1 x: FnType) -> () { return } // expected-error {{cannot be declared public}}
  public subscript(test2 x: main.FnType) -> () { return } // expected-error {{cannot be declared public}}

  public subscript(test1p x: PkgFnType) -> () { return } // expected-error {{cannot be declared public}}
  public subscript(test2p x: main.PkgFnType) -> () { return } // expected-error {{cannot be declared public}}

  package subscript(ptest1 x: FnType) -> () { return } // expected-error {{cannot be declared package}}
  package subscript(ptest2 x: main.FnType) -> () { return } // expected-error {{cannot be declared package}}

  public subscript(reject1 x: FnType?) -> () { return } // expected-error {{cannot be declared public}}
  public subscript(reject2 x: main.FnType?) -> () { return } // expected-error {{cannot be declared public}}
  public subscript(reject3 x: Int) -> FnType { fatalError() } // expected-error {{cannot be declared public}}
  public subscript(reject4 x: Int) -> main.FnType { fatalError() } // expected-error {{cannot be declared public}}

  public subscript(reject1p x: PkgFnType?) -> () { return } // expected-error {{cannot be declared public}}
  public subscript(reject2p x: main.PkgFnType?) -> () { return } // expected-error {{cannot be declared public}}
  public subscript(reject3p x: Int) -> PkgFnType { fatalError() } // expected-error {{cannot be declared public}}
  public subscript(reject4p x: Int) -> main.PkgFnType { fatalError() } // expected-error {{cannot be declared public}}

  package subscript(preject1 x: FnType?) -> () { return } // expected-error {{cannot be declared package}}
  package subscript(preject2 x: main.FnType?) -> () { return } // expected-error {{cannot be declared package}}
  package subscript(preject3 x: Int) -> FnType { fatalError() } // expected-error {{cannot be declared package}}
  package subscript(preject4 x: Int) -> main.FnType { fatalError() } // expected-error {{cannot be declared package}}
}

private struct ActuallyPrivate {} // expected-note * {{declared here}}
private typealias ActuallyPrivateAlias = ActuallyPrivate
package struct ActuallyPackage {} // expected-note * {{declared here}}
private typealias ActuallyPackageAlias = ActuallyPackage

public var failFn: (ActuallyPrivate) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var failFn2: (_ x: ActuallyPrivate) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var failFn3: (main.ActuallyPrivate) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var failFn4: (_ x: main.ActuallyPrivate) -> Void = { _ in } // expected-error {{cannot be declared public}}

public var failFnp: (ActuallyPackage) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var failFn2p: (_ x: ActuallyPackage) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var failFn3p: (main.ActuallyPackage) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var failFn4p: (_ x: main.ActuallyPackage) -> Void = { _ in } // expected-error {{cannot be declared public}}

package var pfailFn: (ActuallyPrivate) -> Void = { _ in } // expected-error {{cannot be declared package}}
package var pfailFn2: (_ x: ActuallyPrivate) -> Void = { _ in } // expected-error {{cannot be declared package}}
package var pfailFn3: (main.ActuallyPrivate) -> Void = { _ in } // expected-error {{cannot be declared package}}
package var pfailFn4: (_ x: main.ActuallyPrivate) -> Void = { _ in } // expected-error {{cannot be declared package}}

public var failNested1: (_ x: (ActuallyPrivate) -> Void) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var failNested2: (_ x: (main.ActuallyPrivate) -> Void) -> Void = { _ in } // expected-error {{cannot be declared public}}
public func failTest(x: ActuallyPrivate) {} // expected-error {{cannot be declared public}}
public func failTest2(x: main.ActuallyPrivate) {} // expected-error {{cannot be declared public}}

public var failNested1p: (_ x: (ActuallyPackage) -> Void) -> Void = { _ in } // expected-error {{cannot be declared public}}
public var failNested2p: (_ x: (main.ActuallyPackage) -> Void) -> Void = { _ in } // expected-error {{cannot be declared public}}
public func failTestp(x: ActuallyPackage) {} // expected-error {{cannot be declared public}}
public func failTest2p(x: main.ActuallyPackage) {} // expected-error {{cannot be declared public}}

package var pfailNested1: (_ x: (ActuallyPrivate) -> Void) -> Void = { _ in } // expected-error {{cannot be declared package}}
package var pfailNested2: (_ x: (main.ActuallyPrivate) -> Void) -> Void = { _ in } // expected-error {{cannot be declared package}}
package func pfailTest(x: ActuallyPrivate) {} // expected-error {{cannot be declared package}}
package func pfailTest2(x: main.ActuallyPrivate) {} // expected-error {{cannot be declared package}}

// Property has an inferred type, public alias with
// private generic parameter bound.
public struct PublicGeneric<T> {}

public typealias GenericAlias<T> = PublicGeneric<T>

fileprivate func makeAValue() -> GenericAlias<ActuallyPrivate> { }
package func makeAValuePkg() -> GenericAlias<ActuallyPackage> { }

public var cannotBePublic = makeAValue()
// expected-error@-1 {{variable cannot be declared public because its type 'GenericAlias<ActuallyPrivate>' (aka 'PublicGeneric<ActuallyPrivate>') uses a private type}}

package var cannotBePackage = makeAValue()
// expected-error@-1 {{variable cannot be declared package because its type 'GenericAlias<ActuallyPrivate>' (aka 'PublicGeneric<ActuallyPrivate>') uses a private type}}

public var cannotBePublicPkg = makeAValuePkg()
// expected-error@-1 {{variable cannot be declared public because its type 'GenericAlias<ActuallyPackage>' (aka 'PublicGeneric<ActuallyPackage>') uses a package type}}
