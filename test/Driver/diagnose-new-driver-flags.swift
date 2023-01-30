// RUN: %swiftc_driver -emit-module -user-module-version 999.999 %s -### 2>&1 | %FileCheck %s

// CHECK: warning: option '-user-module-version' is only supported in swift-driver
