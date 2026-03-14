// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t/B.framework/Modules/B.swiftmodule/%target-swiftmodule-name -module-name B \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -I %t -F %t -import-objc-header %t/Bridging.h \
// RUN:   %t/B.swift

// RUN: %target-swift-frontend -emit-module -o %t/A.framework/Modules/A.swiftmodule/%target-swiftmodule-name -module-name A \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -I %t -F %t -import-objc-header %t/Bridging2.h \
// RUN:   %t/A.swift

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/user.swift -o %t/deps.json -import-objc-header %t/Bridging3.h \
// RUN:   -I %t -F %t

// RUN: %FileCheck %s --input-file=%t/deps.json
// CHECK-DAG: "swiftPrebuiltExternal": "A"
// CHECK-DAG: "swiftPrebuiltExternal": "B"

//--- A.swift
@_exported import A
public func testA() {}

//--- B.swift
@_exported import B
import C
public func testB() {}

//--- user.swift
public func skip() {}

//--- Bridging.h
void nothing(void);

//--- Bridging2.h
#include "B/B.h"

//--- Bridging3.h
#include "A/A.h"

//--- A.framework/Headers/A.h
struct A {
  int a;
};

//--- B.framework/Headers/B.h
void b(void);
@interface B
@end

//--- C.h
void c(void);

//--- A.framework/Modules/module.modulemap
framework module A {
  umbrella header "A.h"
  export *
}

//--- B.framework/Modules/module.modulemap
framework module B {
  umbrella header "B.h"
  export *
}

//--- module.modulemap
module C {
  header "C.h"
  export *
}
