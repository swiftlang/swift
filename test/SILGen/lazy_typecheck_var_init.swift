// RUN: %target-swift-frontend -emit-silgen %s -parse-as-library -enable-library-evolution -module-name Test | %FileCheck %s --check-prefixes=CHECK,CHECK-NON-LAZY
// RUN: %target-swift-frontend -emit-silgen %s -parse-as-library -enable-library-evolution -module-name Test -experimental-lazy-typecheck | %FileCheck %s --check-prefixes=CHECK,CHECK-LAZY

enum E {
  case a, b
}

func internalFunc(_ e: E = .a) -> Int {
  switch e {
  case .a: return 1
  case .b: return 2
  }
}

// CHECK-LABEL: sil private [global_init_once_fn]{{.*}} @$s4Test9globalVar_WZ : $@convention(c) (Builtin.RawPointer) -> () {
public var globalVar = internalFunc()

public struct S {
  // CHECK-LABEL: sil hidden [transparent]{{.*}} @$s4Test1SV11instanceVarSivpfi : $@convention(thin) () -> Int {
  public var instanceVar = internalFunc()

  // CHECK-LABEL: sil hidden [transparent]{{.*}} @$s4Test1SV12instanceVar2Sivpfi : $@convention(thin) () -> Int {
  public var instanceVar2 = internalFunc(.b)

  // CHECK-NOT: s4Test1SV15lazyInstanceVarSivpfi
  // CHECK-LABEL: sil hidden [transparent]{{.*}} @$s4Test1SV018$__lazy_storage_$_B11InstanceVar33_0E4F053AA3AB7D4CDE3A37DBA8EF0430LLSiSgvpfi : $@convention(thin) () -> Optional<Int> {
  public lazy var lazyInstanceVar = internalFunc()

  // CHECK-LABEL: sil private [global_init_once_fn]{{.*}} @$s4Test1SV9staticVar_WZ : $@convention(c) (Builtin.RawPointer) -> () {
  public static var staticVar = internalFunc()

  // FIXME: This initializer should be subsumed.
  // CHECK-LAZY: sil hidden [transparent]{{.*}} @$s4Test1SV15subsumedInitVarSivpfi : $@convention(thin) () -> Int {
  // CHECK-NON-LAZY-NOT: sil hidden [transparent]{{.*}} @$s4Test1SV15subsumedInitVarSivpfi : $@convention(thin) () -> Int {
  public var subsumedInitVar = internalFunc()

  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s4Test1SV19varWithInitAccessorSivpfi : $@convention(thin) () -> Int {
  public var varWithInitAccessor: Int = internalFunc() {
    @storageRestrictions(initializes: subsumedInitVar)
    init {
      subsumedInitVar = newValue
    }
    get { subsumedInitVar }
  }

  // FIXME: Test vars with property wrappers.
}

extension S {
  // CHECK-LABEL: sil private [global_init_once_fn]{{.*}} @$s4Test1SV20staticVarInExtension_WZ : $@convention(c) (Builtin.RawPointer) -> () {
  public static var staticVarInExtension = internalFunc()
}

// CHECK-LABEL: sil{{.*}} @$s4Test8returnsSAA1SVyF : $@convention(thin) () -> @out S
public func returnsS() -> S {
  // Force the synthesized initializer for S to be emitted.
  // CHECK: function_ref @$s4Test1SVACycfC : $@convention(method) (@thin S.Type) -> @out S
  return S()
}
