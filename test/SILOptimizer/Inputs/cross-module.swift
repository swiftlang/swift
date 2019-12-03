import Submodule

private enum PE<T> {
  case A
  case B(T)
}

public struct Container {

  private final class Base {
  }

  @inline(never)
  public func testclass<T>(_ t: T) -> T {
    var arr = Array<Base>()
    arr.append(Base())
    print(arr)
    return t
  }

  @inline(never)
  @_semantics("optimize.sil.specialize.generic.never")
  public func testclass_gen<T>(_ t: T) -> T {
    var arr = Array<Base>()
    arr.append(Base())
    print(arr)
    return t
  }

  @inline(never)
  public func testenum<T>(_ t: T) -> T {
    var arr = Array<PE<T>>()
    arr.append(.B(t))
    print(arr)
    return t
  }

  @inline(never)
  @_semantics("optimize.sil.specialize.generic.never")
  public func testenum_gen<T>(_ t: T) -> T {
    var arr = Array<PE<T>>()
    arr.append(.B(t))
    print(arr)
    return t
  }

  public init() { }
}

private class PrivateBase<T> {
  var t: T
  func foo() -> Int { return 27 }

  init(_ t: T) { self.t = t }
}

private class PrivateDerived<T> : PrivateBase<T> {
  override func foo() -> Int { return 28 }
}

@inline(never)
private func getClass<T>(_ t : T) -> PrivateBase<T> {
  return PrivateDerived<T>(t)
}

@inline(never)
public func createClass<T>(_ t: T) -> Int {
  return getClass(t).foo()
}

@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
public func createClass_gen<T>(_ t: T) -> Int {
  return getClass(t).foo()
}

private struct PrivateError: Error { }

public func returnPrivateError<V>(_ v: V) -> Error {
  return PrivateError()
}

struct InternalError: Error { }

public func returnInternalError<V>(_ v: V) -> Error {
  return InternalError()
}

private protocol PrivateProtocol {
  func foo() -> Int
}

open class OpenClass<T> {
  public init() { }

  @inline(never)
  fileprivate func bar(_ t: T) {
    print(t)
  }
}

extension OpenClass {
  @inline(never)
  public func testit() -> Bool {
    return self is PrivateProtocol
  }
}

@inline(never)
public func checkIfClassConforms<T>(_ t: T) {
  let x = OpenClass<T>()
  print(x.testit())
}

@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
public func checkIfClassConforms_gen<T>(_ t: T) {
  let x = OpenClass<T>()
  print(x.testit())
}

@inline(never)
public func callClassMethod<T>(_ t: T) {
  let k = OpenClass<T>()
  k.bar(t)
}

extension Int : PrivateProtocol {
  func foo() -> Int { return self }
}

@inline(never)
private func printFooExistential(_ p: PrivateProtocol) {
  print(p.foo())
}

@inline(never)
private func printFooGeneric<T: PrivateProtocol>(_ p: T) {
  print(p.foo())
}

@inline(never)
public func callFoo<T>(_ t: T) {
  printFooExistential(123)
  printFooGeneric(1234)
}

@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
public func callFoo_gen<T>(_ t: T) {
  printFooExistential(123)
  printFooGeneric(1234)
}

@inline(never)
public func callGenericSubmoduleFunc<T>(_ t: T) {
  genericSubmoduleFunc(t)
}

@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
public func callGenericSubmoduleFunc_gen<T>(_ t: T) {
  genericSubmoduleFunc(t)
}

@inline(never)
public func genericClosure<T>(_ t: T) -> T {
  let c : () -> T = { return t }
  return c()
}

@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
public func genericClosure_gen<T>(_ t: T) -> T {
  let c : () -> T = { return t }
  return c()
}

struct Abc {
  var x: Int { return 27 }
  var y: Int { return 28 }
}

class Myclass {
  var x: Int { return 27 }
  var y: Int { return 28 }
}

class Derived : Myclass {
  override var x: Int { return 29 }
  override var y: Int { return 30 }
}

@inline(never)
func getStructKeypath<T>(_ t: T) -> KeyPath<Abc, Int> {
  return \Abc.x
}

@inline(never)
public func useStructKeypath<T>(_ t: T) -> Int {
  let abc = Abc()
  return abc[keyPath: getStructKeypath(t)]
}

@inline(never)
func getClassKeypath<T>(_ t: T) -> KeyPath<Myclass, Int> {
  return \Myclass.x
}

@inline(never)
public func useClassKeypath<T>(_ t: T) -> Int {
  let c = Derived()
  return c[keyPath: getClassKeypath(t)]
}

