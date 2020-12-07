// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-prespecialization -emit-module -DLIB_A %s -module-name A -emit-module-path %t/A.swiftmodule
// RUN: %target-swift-frontend -enable-experimental-prespecialization -emit-module -DLIB_B %s -module-name B -emit-module-path %t/B.swiftmodule -I %t
// RUN: %target-swift-frontend -enable-experimental-prespecialization -module-name C -emit-sil -O -DLIB_C %s -I %t | %FileCheck  %s
// RUN: %target-swift-frontend -enable-experimental-prespecialization -module-name C -emit-sil -O -DLIB_C_NO_SPI %s -I %t | %FileCheck  %s --check-prefix=NOSPI

// Test using the public swiftinterface
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-prespecialization -swift-version 5 -enable-library-evolution -emit-module -DLIB_A %s -module-name A -emit-module-path %t/A.swiftmodule -emit-module-interface-path %t/A.swiftinterface
// RUN: %target-swift-frontend -enable-experimental-prespecialization -swift-version 5 -enable-library-evolution -emit-module -DLIB_B %s -module-name B -emit-module-path %t/B.swiftmodule -I %t -emit-module-interface-path %t/B.swiftinterface
// RUN: rm %t/A.swiftmodule %t/B.swiftmodule
// RUN: %target-swift-frontend -enable-experimental-prespecialization -module-name C -emit-sil -O -DLIB_C %s -I %t | %FileCheck  %s --check-prefix=PUBLIC

// Test using the private swiftinterface
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-prespecialization -swift-version 5 -enable-library-evolution -emit-module -DLIB_A %s -module-name A -emit-module-path %t/A.swiftmodule -emit-module-interface-path %t/A.swiftinterface -emit-private-module-interface-path %t/A.private.swiftinterface
// RUN: %target-swift-frontend -enable-experimental-prespecialization -swift-version 5 -enable-library-evolution -emit-module -DLIB_B %s -module-name B -emit-module-path %t/B.swiftmodule -I %t -emit-module-interface-path %t/B.swiftinterface -emit-private-module-interface-path %t/B.private.swiftinterface
// RUN: rm %t/A.swiftmodule %t/B.swiftmodule
// RUN: %target-swift-frontend -enable-experimental-prespecialization -module-name C -emit-sil -O -DLIB_C %s -I %t | %FileCheck  %s

#if LIB_A

@_specialize(exported: true, spi: A, where T == Int)
public func genericFuncLibA<T>(_ t: T) { print(t) }

public struct SomeStruct<T> {
  var x : T

  public init(_ t: T) { self.x = t }

  @_specialize(exported: true, spi: A, where T == Int)
  @inlinable
  public func genericFuncInSomeStruct(_ t: T) { print(t) }
}

public struct SomeClass<T> {
  var x : T

  public init(_ t: T) { self.x = t }

  @_specialize(exported: true, spi: A, where T == Int)
  @inlinable
  public func genericFuncInSomeClass(_ t: T) { print(t) }
}

public enum SomeEnum<T> {
  case A
  case B(T)

  @_specialize(exported: true, spi: A, where T == Int)
  @inlinable
  public func genericFuncInSomeEnum(_ t: T) { print(SomeEnum.B(t)) }
}

@usableFromInline
struct SomeInternalStruct<T> {
  var x : T

  @usableFromInline
  init(_ t: T) { self.x = t }

  @_specialize(exported: true, spi: A, where T == Int)
  @inlinable
  func genericFuncInSomeStruct(_ t: T) { print(t) }
}

@inlinable
@inline(__always)
public func testSomeInternalStruct<T>(_ t: T) {
  SomeInternalStruct(t).genericFuncInSomeStruct(t);
}

#elseif LIB_B

import A

@_specialize(exported: true, spi: A, where T == Int)
public func genericFuncLibB<T>(_ t: T) { print(t) }

@_specializeExtension
extension SomeStruct {
  @_specialize(exported: true, spi: A, target: genericFuncInSomeStruct(_:), where T == Double)
  public func genericFuncInSomeStruct_specialized(_ t: T) { fatalError("don't call") }
}

extension SomeClass {
  @_specialize(exported: true, spi: A, target: genericFuncInSomeClass(_:), where T == Double)
  public func genericFunc_specialized(_ t: T) { fatalError("don't call") }
}

extension SomeEnum {
  @_specialize(exported: true, spi: A, target: genericFuncInSomeEnum(_:), where T == Double)
  public func genericFunc_specialized(_ t: T) { fatalError("don't call") }
}

@_specializeExtension
extension SomeInternalStruct {
  @_specialize(exported: true, spi: A, target: genericFuncInSomeStruct(_:), where T == Double)
  public func genericFuncInSomeStruct_specialized(_ t: T) { fatalError("don't call") }
}

#elseif LIB_C

@_spi(A) import A
@_spi(A) import B

// CHECK-LABEL: sil @$s1C21testUseSpecializedSPIyyF : $@convention(thin) () -> () {
// CHECK:  [[F1:%.*]] = function_ref @$s1A15genericFuncLibAyyxlFSi_Ts5
// CHECK:  apply [[F1]]({{.*}}) : $@convention(thin) (Int) -> ()
// CHECK:  [[F2:%.*]] = function_ref @$s1B15genericFuncLibByyxlFSi_Ts5
// CHECK:  apply [[F2]]({{.*}}) : $@convention(thin) (Int) -> ()
// CHECK:  function_ref @$s1A10SomeStructV013genericFuncInaB0yyxFSi_Ts5
// CHECK:  function_ref @$s1A10SomeStructV013genericFuncInaB0yyxFSd_Ts5
// CHECK: function_ref @$s1A9SomeClassV013genericFuncInaB0yyxFSi_Ts5
// CHECK: function_ref @$s1A9SomeClassV013genericFuncInaB0yyxFSd_Ts5
// CHECK: function_ref @$s1A8SomeEnumO013genericFuncInaB0yyxFSi_Ts5
// CHECK: function_ref @$s1A8SomeEnumO013genericFuncInaB0yyxFSd_Ts5
// CHECK: } // end sil function '$s1C21testUseSpecializedSPIyyF'

// PUBLIC-LABEL: sil @$s1C21testUseSpecializedSPIyyF : $@convention(thin) () -> () {
// PUBLIC-NOT:  function_ref @$s1A15genericFuncLibAyyxlFSi_Ts5
// PUBLIC-NOT:  function_ref @$s1B15genericFuncLibByyxlFSi_Ts5
// PUBLIC-NOT:  function_ref @$s1A10SomeStructV013genericFuncInaB0yyxFSi_Ts5
// PUBLIC-NOT:  function_ref @$s1A10SomeStructV013genericFuncInaB0yyxFSd_Ts5
// PUBLIC-NOT: function_ref @$s1A9SomeClassV013genericFuncInaB0yyxFSi_Ts5
// PUBLIC-NOT: function_ref @$s1A9SomeClassV013genericFuncInaB0yyxFSd_Ts5
// PUBLIC-NOT: function_ref @$s1A8SomeEnumO013genericFuncInaB0yyxFSi_Ts5
// PUBLIC-NOT: function_ref @$s1A8SomeEnumO013genericFuncInaB0yyxFSd_Ts5
// PUBLIC: } // end sil function '$s1C21testUseSpecializedSPIyyF'

public func testUseSpecializedSPI() {
  genericFuncLibA(1)
  genericFuncLibB(2)
  SomeStruct(1).genericFuncInSomeStruct(5)
  SomeStruct(1.0).genericFuncInSomeStruct(5.0)
  SomeClass(1).genericFuncInSomeClass(5)
  SomeClass(1.0).genericFuncInSomeClass(5.0)
  SomeEnum.B(1).genericFuncInSomeEnum(1)
  SomeEnum.B(1.0).genericFuncInSomeEnum(1.0)
  testSomeInternalStruct(1)
  testSomeInternalStruct(1.0)
}

#elseif LIB_C_NO_SPI

import A
import B

// NOSPI-LABEL: sil @$s1C21testUseSpecializedSPIyyF : $@convention(thin) () -> () {
// NOSPI-NOT:  function_ref @$s1A15genericFuncLibAyyxlFSi_Ts5
// NOSPI-NOT:  function_ref @$s1B15genericFuncLibByyxlFSi_Ts5
// NOSPI-NOT:  function_ref @$s1A10SomeStructV013genericFuncInaB0yyxFSi_Ts5
// NOSPI-NOT:  function_ref @$s1A10SomeStructV013genericFuncInaB0yyxFSd_Ts5
// NOSPI-NOT: function_ref @$s1A9SomeClassV013genericFuncInaB0yyxFSi_Ts5
// NOSPI-NOT: function_ref @$s1A9SomeClassV013genericFuncInaB0yyxFSd_Ts5
// NOSPI-NOT: function_ref @$s1A8SomeEnumO013genericFuncInaB0yyxFSi_Ts5
// NOSPI-NOT: function_ref @$s1A8SomeEnumO013genericFuncInaB0yyxFSd_Ts5
// NOSPI: } // end sil function '$s1C21testUseSpecializedSPIyyF'

public func testUseSpecializedSPI() {
  genericFuncLibA(1)
  genericFuncLibB(2)
  SomeStruct(1).genericFuncInSomeStruct(5)
  SomeStruct(1.0).genericFuncInSomeStruct(5.0)
  SomeClass(1).genericFuncInSomeClass(5)
  SomeClass(1.0).genericFuncInSomeClass(5.0)
  SomeEnum.B(1).genericFuncInSomeEnum(1)
  SomeEnum.B(1.0).genericFuncInSomeEnum(1.0)
  testSomeInternalStruct(1)
  testSomeInternalStruct(1.0)
}
#endif
