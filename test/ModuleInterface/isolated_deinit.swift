// RUN: %target-swift-frontend -emit-module %s \
// RUN:   -module-name A -swift-version 5 \
// RUN:   -disable-availability-checking \
// RUN:   -enable-library-evolution \
// RUN:     -emit-module-path %t/A.swiftmodule \
// RUN:     -emit-module-interface-path %t/A.swiftinterface

// RUN: %FileCheck %s < %t/A.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/A.swiftinterface)

// REQUIRES: concurrency

// CHECK: @_Concurrency.MainActor @preconcurrency public class C1 {
// CHECK:   {{(@objc )?}} isolated deinit
// CHECK: }

@MainActor
@preconcurrency
public class C1 {
  isolated deinit {
  }
}

// CHECK: @preconcurrency public class C2 {
// CHECK:   {{(@objc )?}} @_Concurrency.MainActor deinit
// CHECK: }

@preconcurrency
public class C2 {
  @MainActor deinit {
  }
}
