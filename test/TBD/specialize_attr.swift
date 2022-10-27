// REQUIRES: VENDOR=apple

// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing

// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing -O %s
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing -O %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing -O
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing -O

@_specialize(exported: false, where T == Int)
@_specialize(exported: true, where T == Float)
public func foo<T>(_ x : T) -> T { return x }

@_specialize(exported: false, where T == Int)
@_specialize(exported: true, where T == Float)
internal func fooInternal<T>(_ x : T) -> T { return x }

@_specialize(exported: false, where T == Int)
@_specialize(exported: true, where T == Float)
private func fooPrivate<T>(_ x : T) -> T { return x }

public struct S {
  @_specialize(exported: false, where T == Int)
  @_specialize(exported: true, where T == Float)
  public func foo<T>(_ x : T) -> T { return x }

  @_specialize(exported: false, where T == Int)
  @_specialize(exported: true, where T == Float)
  internal func fooInternal<T>(_ x : T) -> T { return x }

  @_specialize(exported: false, where T == Int)
  @_specialize(exported: true, where T == Float)
  private func fooPrivate<T>(_ x : T) -> T { return x }
}

public class C {
  @_specialize(exported: false, where T == Int)
  @_specialize(exported: true, where T == Float)
  public func foo<T>(_ x : T) -> T { return x }

  @_specialize(exported: false, where T == Int)
  @_specialize(exported: true, where T == Float)
  internal func fooInternal<T>(_ x : T) -> T { return x }

  @_specialize(exported: false, where T == Int)
  @_specialize(exported: true, where T == Float)
  private func fooPrivate<T>(_ x : T) -> T { return x }
}
