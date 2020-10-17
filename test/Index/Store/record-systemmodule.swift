// --- Prepare SDK (.swiftmodule).
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/SDK)
// RUN: mkdir -p %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule
// RUN: %target-swift-frontend \
// RUN:     -emit-module \
// RUN:     -module-name SomeModule \
// RUN:     -o %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     -swift-version 5 \
// RUN:     -D SOME_MODULE \
// RUN:     %s

#if SOME_MODULE

public func someFunc() {}

public class C2 {}

extension C2 {
  public func publicFunc() {}
}

// Don't record extensions with nothing to index.
extension C2 {}

extension C2 {
  internal func SECRET() {}
  private func SECRET1() {}
  fileprivate func SECRET2() {}
}

internal protocol SECRETProto {}
extension C2: SECRETProto {}

// -----------------------------------------------------------------------------
// Test-1 - '.swiftmodule' - Normal index-while-building.
//
// RUN: %empty-directory(%t/idx)
// RUN: %empty-directory(%t/modulecache)
//
// --- Built with indexing
// RUN: %target-swift-frontend \
// RUN:     -typecheck \
// RUN:     -index-system-modules \
// RUN:     -index-ignore-stdlib \
// RUN:     -index-store-path %t/idx \
// RUN:     -sdk %t/SDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     -D CLIENT \
// RUN:     %s

#elseif CLIENT

import SomeModule
print(someFunc())

#endif

// -----------------------------------------------------------------------------
// --- Check the records.
// RUN: c-index-test core -print-record %t/idx | %FileCheck %s

// CHECK: 0:0 | function/Swift | s:10SomeModule8someFuncyyF | Def | rel: 0
// CHECK-NEXT: 0:0 | class/Swift | [[class_USR:s:10SomeModule2C2C]] | Def | rel: 0
// CHECK-NEXT: 0:0 | class/Swift | [[class_USR]] | Ref,RelExt | rel: 1
// CHECK-NEXT: 	RelExt | s:e:[[publicFunc_USR:s:10SomeModule2C2C10publicFuncyyF]]
// CHECK-NEXT: 0:0 | instance-method/Swift | [[publicFunc_USR]] | Def,Dyn,RelChild | rel: 1
// CHECK-NEXT: 	RelChild | s:e:[[publicFunc_USR]]
// CHECK-NEXT: 0:0 | extension/ext-class/Swift | s:e:[[publicFunc_USR]] | Def | rel: 0
// CHECK-NOT: SECRET
