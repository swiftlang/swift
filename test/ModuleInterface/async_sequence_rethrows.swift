// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name _Concurrency -disable-implicit-concurrency-module-import
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name _Concurrency -disable-implicit-concurrency-module-import
// RUN: %FileCheck %s < %t.swiftinterface

// REQUIRES: concurrency

// CHECK-NOT: @rethrows
// CHECK: public protocol AsyncIteratorProtocol
public protocol AsyncIteratorProtocol {
}
