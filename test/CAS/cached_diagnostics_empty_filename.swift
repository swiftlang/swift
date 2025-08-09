// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O -I %t \
// RUN:   -disable-implicit-string-processing-module-import \
// RUN:   %t/test.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas -module-load-mode prefer-serialized

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:_SwiftConcurrencyShims > %t/cshim.cmd
// RUN: %swift_frontend_plain @%t/cshim.cmd

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CHECK --check-prefix=CACHE-MISS

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CHECK --check-prefix=CACHE-HIT

// CACHE-MISS: remark: cache miss for input
// CHECK: <module-includes>:1
// CHECK-SAME: note: in file included from <module-includes>:1:
// CHECK: warning: warning a.h
// CACHE-HIT: remark: replay output file

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-swift-modules\"" >> %t/MyApp.cmd
// RUN: echo "\"-explicit-swift-module-map-file\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/map.casid\"" >> %t/MyApp.cmd

// RUN: %target-swift-frontend  -cache-compile-job -module-name Test -O -cas-path %t/cas @%t/MyApp.cmd %t/test.swift \
// RUN:   -emit-module -o %t/test.swiftmodule -require-explicit-sendable -strict-concurrency=complete

//--- module.modulemap
module A {
  header "a.h"
  export *
}

//--- a.h
#warning warning a.h

#pragma clang attribute push(__attribute__((swift_attr("@_nonSendable(_assumed)"))), apply_to = any(objc_interface, record, enum))
@protocol P
@end

@interface C
@end
#pragma clang attribute pop

//--- test.swift
import A
import _Concurrency

func acceptSendable<T: Sendable>(_: T) {
}

func testMe(c: C) {
  acceptSendable(c)
}

