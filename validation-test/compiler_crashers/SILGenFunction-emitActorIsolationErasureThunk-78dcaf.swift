// {"signature":"swift::Lowering::SILGenFunction::emitActorIsolationErasureThunk(swift::SILLocation, swift::Lowering::ManagedValue, swift::CanTypeWrapper<swift::AnyFunctionType>, swift::CanTypeWrapper<swift::AnyFunctionType>)","signatureAssert":"Assertion failed: (!expectedType->hasErasedIsolation()), function emitActorIsolationErasureThunk"}
//
// RUN: %empty-directory(%t)
//
// RUN: %target-swift-frontend -emit-module -module-name Lib -emit-module-path %t/Lib.swiftmodule -DLib %s \
// RUN:   -language-mode 5
// RUN: not --crash %target-swift-emit-silgen -I %t %s \
// RUN:   -language-mode 6
//
// rdar://171146729
#if Lib
  public struct Foo {
    public func bar(_: @isolated(any) @Sendable (Foo) -> Void) {
    }
  }
#else
  import Lib
  func test(foo: Foo) {
    foo.bar { @MainActor _ in
    }
  }
#endif
