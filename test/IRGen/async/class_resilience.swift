// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-experimental-concurrency -enable-library-evolution -emit-module-path=%t/resilient_class.swiftmodule -module-name=resilient_class %S/Inputs/resilient_class.swift
// RUN: %target-swift-frontend -I %t -emit-ir -enable-experimental-concurrency -enable-library-evolution %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-%target-cpu %s
// REQUIRES: concurrency

import resilient_class

open class MyBaseClass<T> {
  var value: T

  open func wait() async -> Int {
    return 0
  }

  open func wait() async -> T {
    return value
  }

  open func waitThrows() async throws -> Int {
    return 0
  }

  open func waitThrows() async throws -> T {
    return value
  }

  // FIXME
  // open func waitGeneric<T>(_: T) async -> T
  // open func waitGenericThrows<T>(_: T) async throws -> T

  public init(_ value: T) {
    self.value = value
  }
}

// CHECK-LABEL: @"$s16class_resilience11MyBaseClassC4waitxyYFTjTu" = {{(dllexport )?}}{{(protected )?}}global %swift.async_func_pointer

// CHECK-LABEL: @"$s16class_resilience11MyBaseClassCMn" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-SAME: %swift.async_func_pointer* @"$s16class_resilience11MyBaseClassC4waitxyYFTu"

// CHECK-LABEL: @"$s16class_resilience9MyDerivedCMn" = hidden constant
// CHECK-SAME: %swift.async_func_pointer* @"$s16class_resilience9MyDerivedC4waitSiyYF010resilient_A09BaseClassCADxyYFTVTu"

// CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swift{{(tail)?}}cc void @"$s16class_resilience14callsAwaitableyx010resilient_A09BaseClassCyxGYlF"(%swift.opaque* noalias nocapture %0, %swift.context* swiftasync %1{{.*}})
// CHECK: %swift.async_func_pointer* @"$s15resilient_class9BaseClassC4waitxyYFTjTu"
// CHECK: ret void
public func callsAwaitable<T>(_ c: BaseClass<T>) async -> T {
  return await c.wait()
}

// CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swift{{(tail)?}}cc void @"$s16class_resilience11MyBaseClassC4waitxyYFTj"(%swift.opaque* noalias nocapture %0, %swift.context* swiftasync %1, %T16class_resilience11MyBaseClassC* swiftself %2) {{#([0-9]+)}} {

class MyDerived : BaseClass<Int> {
  override func wait() async -> Int {
    return await super.wait()
  }
}
