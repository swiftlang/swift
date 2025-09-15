// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-silgen %s -parse-as-library -enable-library-evolution -module-name Test | %FileCheck %s --check-prefixes=CHECK,CHECK-NO-SKIP
// RUN: %target-swift-frontend -emit-silgen %s -parse-as-library -enable-library-evolution -module-name Test -experimental-skip-non-exportable-decls | %FileCheck %s --check-prefixes=CHECK,CHECK-SKIP

import Swift

#warning("!")

// CHECK-NO-SKIP: sil_global private @$s4Test17internalGlobalVar_Wz : $Builtin.Word
// CHECK-SKIP-NOT: s4Test17internalGlobalVar_Wz

// CHECK-NO-SKIP: sil_global hidden @$s4Test17internalGlobalVarSivp : $Int
// CHECK-SKIP-NOT: s4Test17internalGlobalVarSivp

// CHECK-NO-SKIP: sil_global private @$s4Test15publicGlobalVar_Wz : $Builtin.Word
// CHECK-SKIP-NOT: s4Test15publicGlobalVar_Wz

// CHECK: sil_global private @$s4Test15publicGlobalVarSivp : $Int

// CHECK-NO-SKIP: sil private{{.*}} @$s4Test11privateFunc33_CFB3F9DC47F5EF9E1D08B58758351A08LLyyF : $@convention(thin) () -> () {
// CHECK-SKIP-NOT: s4Test11privateFunc33_CFB3F9DC47F5EF9E1D08B58758351A08LLyyF
private func privateFunc() {}

// CHECK-NO-SKIP: sil hidden{{.*}} @$s4Test12internalFuncyyF : $@convention(thin) () -> () {
// CHECK-SKIP-NOT: s4Test12internalFuncyyF
internal func internalFunc() {}

// CHECK-NO-SKIP: sil hidden{{.*}} @$s4Test022internalFuncWithNestedC0yyF : $@convention(thin) () -> () {
// CHECK-SKIP-NOT: s4Test022internalFuncWithNestedC0yyF
internal func internalFuncWithNestedFunc() {
  lazy var x = 1
  defer { internalFunc() }
  func nested() {}
  nested()
  let _: () -> () = {
    defer { internalFunc() }
    internalFunc()
  }()
}
// CHECK-NO-SKIP: sil private{{.*}} @$s4Test022internalFuncWithNestedC0yyF1xL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }) -> Int
// CHECK-SKIP-NOT: s4Test022internalFuncWithNestedC0yyF1xL_Sivg

// CHECK-NO-SKIP: sil private{{.*}} @$s4Test022internalFuncWithNestedC0yyF6$deferL_yyF : $@convention(thin) () -> () {
// CHECK-SKIP-NOT: s4Test022internalFuncWithNestedC0yyF6$deferL_yyF

// CHECK-NO-SKIP: sil private{{.*}} @$s4Test022internalFuncWithNestedC0yyF6nestedL_yyF : $@convention(thin) () -> () {
// CHECK-SKIP-NOT: s4Test022internalFuncWithNestedC0yyF6nestedL_yyF

// CHECK-NO-SKIP: sil private{{.*}} @$s4Test022internalFuncWithNestedC0yyFyycyXEfU_ : $@convention(thin) () -> @owned @callee_guaranteed () -> () {
// CHECK-SKIP-NOT: s4Test022internalFuncWithNestedC0yyFyycyXEfU_

// CHECK-NO-SKIP: sil private{{.*}} @$s4Test022internalFuncWithNestedC0yyFyycyXEfU_6$deferL_yyF : $@convention(thin) () -> () {
// CHECK-SKIP-NOT: sil{{.*}} @$s4Test022internalFuncWithNestedC0yyFyycyXEfU_6$deferL_yyF

// CHECK: sil{{.*}} @$s4Test10publicFuncyyF : $@convention(thin) () -> () {
public func publicFunc() {}

// CHECK: sil{{.*}} @$s4Test25publicFuncWithNestedFuncsyyF : $@convention(thin) () -> () {
public func publicFuncWithNestedFuncs() {
  lazy var x = 1
  defer { publicFunc() }
  func nested() {}
  nested()
}
// CHECK: sil private{{.*}} @$s4Test25publicFuncWithNestedFuncsyyF1xL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }) -> Int
// CHECK: sil private{{.*}} @$s4Test25publicFuncWithNestedFuncsyyF6$deferL_yyF : $@convention(thin) () -> () {
// CHECK: sil private{{.*}} @$s4Test25publicFuncWithNestedFuncsyyF6nestedL_yyF : $@convention(thin) () -> () {

// CHECK: sil [serialized]{{.*}} @$s4Test13inlinableFuncyyF : $@convention(thin) () -> () {
@inlinable internal func inlinableFunc() {}

// CHECK-NO-SKIP: sil private [global_init_once_fn]{{.*}} @$s4Test17internalGlobalVar_WZ : $@convention(c) (Builtin.RawPointer) -> () {
// CHECK-SKIP-NOT: s4Test17internalGlobalVar_WZ

// CHECK-NO-SKIP: sil hidden [global_init]{{.*}} @$s4Test17internalGlobalVarSivau : $@convention(thin) () -> Builtin.RawPointer {
// CHECK-SKIP-NOT: s4Test17internalGlobalVarSivau
internal var internalGlobalVar = 1

// CHECK-NO-SKIP: sil private [global_init_once_fn]{{.*}} @$s4Test15publicGlobalVar_WZ : $@convention(c) (Builtin.RawPointer) -> () {
// CHECK-SKIP-NOT: s4Test15publicGlobalVar_WZ

// CHECK-NO-SKIP: sil hidden [global_init]{{.*}} @$s4Test15publicGlobalVarSivau : $@convention(thin) () -> Builtin.RawPointer {
// CHECK-SKIP-NOT: sil{{.*}} s4Test15publicGlobalVarSivau
public var publicGlobalVar = 1

// CHECK: sil [serialized]{{.*}} @$s4Test023inlinableFuncWithNestedC0yyF : $@convention(thin) () -> () {
@inlinable internal func inlinableFuncWithNestedFunc() {
  defer { publicFunc() }
  func nested() {}
  nested()
  let _: () -> () = {
    defer { publicFunc() }
    publicFunc()
  }()
}
// CHECK: sil shared [serialized]{{.*}} @$s4Test023inlinableFuncWithNestedC0yyF6$deferL_yyF : $@convention(thin) () -> () {
// CHECK: sil shared [serialized]{{.*}} @$s4Test023inlinableFuncWithNestedC0yyF6nestedL_yyF : $@convention(thin) () -> () {
// CHECK: sil shared [serialized]{{.*}} @$s4Test023inlinableFuncWithNestedC0yyFyycyXEfU_ : $@convention(thin) () -> @owned @callee_guaranteed () -> () {
// CHECK: sil shared [serialized]{{.*}} @$s4Test023inlinableFuncWithNestedC0yyFyycyXEfU_6$deferL_yyF : $@convention(thin) () -> () {

@propertyWrapper
public struct PublicWrapper<T> {
  // CHECK: sil{{.*}} @$s4Test13PublicWrapperV12wrappedValuexvg : $@convention(method) <T> (@in_guaranteed PublicWrapper<T>) -> @out T {
  // CHECK: sil{{.*}} @$s4Test13PublicWrapperV12wrappedValuexvs : $@convention(method) <T> (@in T, @inout PublicWrapper<T>) -> () {
  // CHECK: sil{{.*}} @$s4Test13PublicWrapperV12wrappedValuexvM : $@yield_once @convention(method) <T> (@inout PublicWrapper<T>) -> @yields @inout T {
  public var wrappedValue: T

  // CHECK: sil{{.*}} @$s4Test13PublicWrapperV12wrappedValueACyxGx_tcfC : $@convention(method) <T> (@in T, @thin PublicWrapper<T>.Type) -> @out PublicWrapper<T> {
  public init(wrappedValue: T) { self.wrappedValue = wrappedValue }
}

private class PrivateClass {
  // CHECK-NO-SKIP: sil private{{.*}} @$s4Test12PrivateClass33_CFB3F9DC47F5EF9E1D08B58758351A08LLCfd : $@convention(method) (@guaranteed PrivateClass) -> @owned Builtin.NativeObject {
  // CHECK-SKIP-NOT: s4Test12PrivateClass33_CFB3F9DC47F5EF9E1D08B58758351A08LLCfd

  // CHECK-NO-SKIP: sil private{{.*}} @$s4Test12PrivateClass33_CFB3F9DC47F5EF9E1D08B58758351A08LLCfD : $@convention(method) (@owned PrivateClass) -> () {
  // CHECK-SKIP-NOT: s4Test12PrivateClass33_CFB3F9DC47F5EF9E1D08B58758351A08LLCfD

  // CHECK-NO-SKIP: sil private{{.*}} @$s4Test12PrivateClass33_CFB3F9DC47F5EF9E1D08B58758351A08LLCADycfC : $@convention(method) (@thick PrivateClass.Type) -> @owned PrivateClass {
  // CHECK-SKIP-NOT: s4Test12PrivateClass33_CFB3F9DC47F5EF9E1D08B58758351A08LLCADycfC

  // CHECK-NO-SKIP: sil private{{.*}} @$s4Test12PrivateClass33_CFB3F9DC47F5EF9E1D08B58758351A08LLCADycfc : $@convention(method) (@owned PrivateClass) -> @owned PrivateClass {
  // CHECK-SKIP-NOT: s4Test12PrivateClass33_CFB3F9DC47F5EF9E1D08B58758351A08LLCADycfc
}

public class PublicClass {
  // CHECK-NO-SKIP: sil{{.*}} @$s4Test11PublicClassC11internalVarSivpfi : $@convention(thin) () -> Int {
  // CHECK-SKIP-NOT: s4Test11PublicClassC11internalVarSivpfi
  // CHECK-NO-SKIP: sil hidden{{.*}} @$s4Test11PublicClassC11internalVarSivg : $@convention(method) (@guaranteed PublicClass) -> Int {
  // CHECK-SKIP-NOT: s4Test11PublicClassC11internalVarSivg
  // CHECK-NO-SKIP: sil hidden{{.*}} @$s4Test11PublicClassC11internalVarSivs : $@convention(method) (Int, @guaranteed PublicClass) -> () {
  // CHECK-SKIP-NOT: s4Test11PublicClassC11internalVarSivs
  // CHECK-NO-SKIP: sil hidden{{.*}} @$s4Test11PublicClassC11internalVarSivM : $@yield_once @convention(method) (@guaranteed PublicClass) -> @yields @inout Int {
  // CHECK-SKIP-NOT: s4Test11PublicClassC11internalVarSivM
  var internalVar = 1

  // CHECK-NO-SKIP: sil{{.*}} @$s4Test11PublicClassC9publicVarSivpfi : $@convention(thin) () -> Int {
  // CHECK-SKIP-NOT: s4Test11PublicClassC9publicVarSivpfi
  // CHECK: sil{{.*}} @$s4Test11PublicClassC9publicVarSivg : $@convention(method) (@guaranteed PublicClass) -> Int {
  // CHECK: sil{{.*}} @$s4Test11PublicClassC9publicVarSivs : $@convention(method) (Int, @guaranteed PublicClass) -> () {
  // CHECK: sil{{.*}} @$s4Test11PublicClassC9publicVarSivM : $@yield_once @convention(method) (@guaranteed PublicClass) -> @yields @inout Int {
  public var publicVar = 1

  // CHECK-NO-SKIP: sil{{.*}} @$s4Test11PublicClassC16publicWrappedVarSivpfP : $@convention(thin) (Int) -> @out PublicWrapper<Int> {
  // CHECK-SKIP-NOT: s4Test11PublicClassC16publicWrappedVarSivpfP
  // CHECK: sil{{.*}} @$s4Test11PublicClassC16publicWrappedVarSivg : $@convention(method) (@guaranteed PublicClass) -> Int {
  // CHECK: sil{{.*}} @$s4Test11PublicClassC16publicWrappedVarSivs : $@convention(method) (Int, @guaranteed PublicClass) -> () {
  // CHECK: sil{{.*}} @$s4Test11PublicClassC16publicWrappedVarSivM : $@yield_once @convention(method) (@guaranteed PublicClass) -> @yields @inout Int {
  @PublicWrapper public var publicWrappedVar = publicGlobalVar

  // CHECK-NO-SKIP: sil hidden{{.*}} @$s4Test11PublicClassC14internalMethodyyF : $@convention(method) (@guaranteed PublicClass) -> () {
  // CHECK-SKIP-NOT: s4Test11PublicClassC14internalMethodyyF
  internal func internalMethod() {}

  // CHECK: sil{{.*}} @$s4Test11PublicClassCfd : $@convention(method) (@guaranteed PublicClass) -> @owned Builtin.NativeObject {

  // CHECK: sil{{.*}} @$s4Test11PublicClassCfD : $@convention(method) (@owned PublicClass) -> () {

  // CHECK-NO-SKIP: sil hidden{{.*}} @$s4Test11PublicClassCACycfC : $@convention(method) (@thick PublicClass.Type) -> @owned PublicClass {
  // CHECK-SKIP-NOT: s4Test11PublicClassCACycfC

  // CHECK-NO-SKIP: sil hidden{{.*}} @$s4Test11PublicClassCACycfc : $@convention(method) (@owned PublicClass) -> @owned PublicClass {
  // CHECK-SKIP-NOT: s4Test11PublicClassCACycfc
}

extension PublicClass {
  // CHECK-NO-SKIP: sil hidden{{.*}} @$s4Test11PublicClassC25internalMethodInExtensionyyF : $@convention(method) (@guaranteed PublicClass) -> () {
  // CHECK-SKIP-NOT: s4Test11PublicClassC25internalMethodInExtensionyyF
  internal func internalMethodInExtension() {}
}

@frozen public struct FrozenPublicStruct {
  // CHECK: sil non_abi [transparent] [serialized]{{.*}} @$s4Test18FrozenPublicStructV11internalVarSivpfi : $@convention(thin) () -> Int {
  var internalVar = 1
}

// CHECK-NO-SKIP-LABEL: sil_vtable PrivateClass {
// CHECK-NO-SKIP-NEXT:    #PrivateClass.init!allocator
// CHECK-NO-SKIP-NEXT:    #PrivateClass.deinit!deallocator
// CHECK-NO-SKIP-NEXT:  }
// CHECK-SKIP-NOT:      sil_vtable PrivateClass

// CHECK-LABEL:         sil_vtable PublicClass {
// CHECK-NO-SKIP-NEXT:    #PublicClass.internalVar!getter
// CHECK-SKIP-NOT:        #PublicClass.internalVar!getter
// CHECK-NO-SKIP-NEXT:    #PublicClass.internalVar!setter
// CHECK-SKIP-NOT:        #PublicClass.internalVar!setter
// CHECK-NO-SKIP-NEXT:    #PublicClass.internalVar!modify
// CHECK-SKIP-NOT:        #PublicClass.internalVar!modify
// CHECK-NEXT:            #PublicClass.publicVar!getter
// CHECK-NEXT:            #PublicClass.publicVar!setter
// CHECK-NEXT:            #PublicClass.publicVar!modify
// CHECK-NEXT:            #PublicClass.publicWrappedVar!getter
// CHECK-NEXT:            #PublicClass.publicWrappedVar!setter
// CHECK-NEXT:            #PublicClass.publicWrappedVar!modify
// CHECK-NO-SKIP-NEXT:    #PublicClass.internalMethod
// CHECK-SKIP-NOT:        #PublicClass.internalMethod
// CHECK-NO-SKIP-NEXT:    #PublicClass.init!allocator
// CHECK-SKIP-NOT:        #PublicClass.init!allocator
// CHECK-NEXT:            #PublicClass.deinit!deallocator
// CHECK-NEXT:          }

// CHECK:               sil_property #PublicClass.publicVar
