@_exported import cake

public protocol P1 {}
public protocol P2 {}
public protocol P3: P2, P1 {}

@frozen
public struct S1: P1 {
  public static func foo1() {}
  mutating public func foo2() {}
  internal func foo3() {}
  private func foo4() {}
  fileprivate func foo5() {}
  public func foo6() -> Void {}
}

extension S1: P2 {}

public class C0<T1, T2, T3> {}

public typealias C0Alias = C0<S1, S1, S1>

public class C1: C0Alias {
	open class func foo1() {}
	public weak var Ins : C1?
	public unowned var Ins2 : C1 = C1()
}

public extension C0 where T1 == S1, T2 == S1, T3 == S1 {
  func conditionalFooExt() {}
}

public extension C0 {
  func unconditionalFooExt() {}
}

public func foo1(_ a: Int = 1, b: S1) {}
public func foo2(_ a: Int = #line, b: S1) {}

public enum Number: Int {
  case one
}

public func foo3(_ a: [Int: String]) {}

public extension Int {
  public func foo() {}
}

@frozen
public struct fixedLayoutStruct {
  public var a = 1
  private var b = 2 { didSet {} willSet(value) {} }
  var c = 3
  @available(*, unavailable)
  public let unavailableProperty = 1
}

extension Int: P1 { public func bar() {} }

public protocol ProWithAssociatedType {
  associatedtype A
  associatedtype B = Int
}

public protocol SubsContainer {
  subscript(getter i: Int) -> Int { get }
  subscript(setter i: Int) -> Int { get set }
}

public extension ProWithAssociatedType {
  func NonReqFunc() {}
  var NonReqVar: Int { return 1 }
  typealias NonReqAlias = Int
}

public protocol PSuper {
  associatedtype T
  func foo()
}

public protocol PSub: PSuper {
  associatedtype T
  func foo()
}

public let GlobalVar = 1

public extension P1 {
  static func +(lhs: P1, rhs: P1) -> P1 { return lhs }
}

infix operator ..*..

@usableFromInline
@_fixed_layout
class UsableFromInlineClass {
  private var Prop = 1
}

class InternalType {}

extension InternalType {}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
public extension PSuper {
  func futureFoo() {}
}

public class FutureContainer {
  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public func futureFoo() {}
  @available(macOS 10.15, *)
  public func NotfutureFoo() {}
}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
extension FutureContainer: P1 {}

extension FutureContainer: P2 {}

@available(macOS 10.1, iOS 10.2, tvOS 10.3, watchOS 3.4, *)
public class PlatformIntroClass {}

@available(swift, introduced: 5)
public class SwiftIntroClass {}

@objc(NewObjCClass)
public class SwiftObjcClass {
  @objc(ObjCFool:ObjCA:ObjCB:)
  public func foo(a:Int, b:Int, c: Int) {}
}

@available(iOS 10.2, tvOS 10.3, watchOS 3.4, *)
@available(macOS, unavailable)
public class UnavailableOnMac {}

@available(macOS, unavailable)
extension SwiftObjcClass {
  public func functionUnavailableOnMac() {}
}
