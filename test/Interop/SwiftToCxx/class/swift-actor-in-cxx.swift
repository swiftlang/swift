// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Actor -clang-header-expose-decls=has-expose-attr -emit-clang-header-path %t/actor.h -disable-availability-checking
// RUN: %FileCheck %s < %t/actor.h

// RUN: %check-interop-cxx-header-in-clang(%t/actor.h)

// RUN: %target-swift-frontend %s -typecheck -module-name Actor -enable-library-evolution -clang-header-expose-decls=has-expose-attr -emit-clang-header-path %t/actor-evo.h -disable-availability-checking
// RUN: %FileCheck %s < %t/actor-evo.h

// RUN: %check-interop-cxx-header-in-clang(%t/actor-evo.h)

// REQUIRES: concurrency

@_expose(Cxx)
public final actor ActorWithField {
  var field: Int64

  public init() {
    field = 0
    print("init ActorWithField")
  }
  deinit {
    print("destroy ActorWithField")
  }

  public func isolatedMethod() {}

  public nonisolated func method() {
      print("nonisolated method")
  }
}

// CHECK: namespace Actor __attribute__((swift_private)) {
// CHECK: SWIFT_EXTERN void * _Nonnull $s5Actor0A9WithFieldCACycfC(SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // init()
// CHECK: SWIFT_EXTERN void $s5Actor0A9WithFieldC6methodyyF(SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // method()

// CHECK: class ActorWithField final : public swift::_impl::RefCountedClass {
// CHECK:   static inline ActorWithField init();
// CHECK:   inline void method();

@_expose(Cxx)
public func takeActorWithIntField(_ x: ActorWithField) {
    print("takeActorWithIntField")
}
