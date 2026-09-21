// REQUIRES: !legacy_swift_driver

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: env SWIFT_ENABLE_COMPILE_CACHE_REMARK=1 %swiftc_driver -module-name Test -O -c -explicit-module-build -module-cache-path %t/clang-module-cache \
// RUN:   -Xfrontend -disable-implicit-string-processing-module-import -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -parse-stdlib \
// RUN:   %t/main.swift -o %t/main.o -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include 2>&1 | %FileCheck %s --check-prefix=CACHE-MISS

// RUN: %empty-directory(%t/clang-module-cache)
// RUN: env SWIFT_ENABLE_COMPILE_CACHE_REMARK=1 %swiftc_driver -module-name Test -O -c -explicit-module-build -module-cache-path %t/clang-module-cache \
// RUN:   -Xfrontend -disable-implicit-string-processing-module-import -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -parse-stdlib \
// RUN:   %t/main.swift -o %t/main.o -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include 2>&1 | %FileCheck %s --check-prefix=CACHE-HIT

// CACHE-MISS-COUNT-3: remark: cache miss
// CACHE-MISS-NOT: remark: cache hit

// CACHE-HIT-COUNT-3: remark: cache hit
// CACHE-HIT-NOT: remark: cache miss

//--- main.swift
import A
import C

//--- single.swift

//--- include/module.modulemap
module A {
  header "A.h"
  export *
}

//--- include/A.h
void a(void);

//--- include/C.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name C -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func c() { }
