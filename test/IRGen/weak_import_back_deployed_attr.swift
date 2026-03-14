// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Library.swiftmodule -parse-as-library %t/Library.swift -enable-library-evolution

// RUN: %target-swift-frontend -primary-file %t/Client.swift -I %t -emit-ir -target %target-cpu-apple-macosx10.50 | %FileCheck %t/Client.swift --check-prefix=CHECK-OLD
// RUN: %target-swift-frontend -primary-file %t/Client.swift -I %t -emit-ir -target %target-cpu-apple-macosx10.60 | %FileCheck %t/Client.swift --check-prefix=CHECK-NEW

// REQUIRES: OS=macosx

//--- Library.swift

@available(macOS 10.50, *)
@backDeployed(before: macOS 10.60)
public func backDeployedFunc() {}

//--- Client.swift

import Library

// CHECK-OLD: declare extern_weak {{.*}} void @"$s7Library16backDeployedFuncyyF"()
// CHECK-NEW: declare {{.*}} void @"$s7Library16backDeployedFuncyyF"()
backDeployedFunc()
