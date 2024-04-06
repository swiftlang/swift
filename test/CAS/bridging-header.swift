// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t/temp.swiftmodule -module-name Test -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap -import-objc-header %t/Bridging.h \
// RUN:   %t/test.swift

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap -import-objc-header %t/Bridging.h

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json Test bridgingHeader | %FileCheck %s

// CHECK:       "includeTree"
// CHECK-NEXT:  "moduleDependencies": [
// CHECK-NEXT:    "A"
// CHECK-NEXT:  ],
// CHECK-NEXT:  "commandLine": [
// CHECK:         "-fmodule-format=obj"
// CHECK:         "-dwarf-ext-refs"
// CHECK:         "-fmodule-file-cache-key",
// CHECK-NEXT:    "-Xcc",
// CHECK-NEXT:    "{{.*}}{{/|\\}}A-{{.*}}.pcm", 
// CHECK-NEXT:    "-Xcc",
// CHECK-NEXT:    "llvmcas://{{.*}}", 
// CHECK-NEXT:    "-Xcc",
// CHECK-NEXT:    "-fmodule-file-cache-key",
// CHECK-NEXT:    "-Xcc",
// CHECK-NEXT:    "{{.*}}{{/|\\}}B-{{.*}}.pcm", 
// CHECK-NEXT:    "-Xcc",
// CHECK-NEXT:    "llvmcas://{{.*}}"

/// Try build then import from a non-caching compilation.

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:B > %t/B.cmd
// RUN: %swift_frontend_plain @%t/B.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json bridgingHeader | tail -n +2  > %t/header.cmd
// RUN: %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules %t/Bridging.h -O -o %t/bridging.pch
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules %t/Bridging.h -O -o %t/bridging.pch > %t/keys.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys.json %t/Bridging.h > %t/key

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-swift-modules\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/bridging.pch\"" >> %t/MyApp.cmd
// RUN: echo "\"-bridging-header-pch-key\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/key\"" >> %t/MyApp.cmd
// RUN: echo "\"-explicit-swift-module-map-file\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/map.casid\"" >> %t/MyApp.cmd

// RUN: %target-swift-frontend  -cache-compile-job -module-name Test -O -cas-path %t/cas @%t/MyApp.cmd %t/test.swift \
// RUN:   -emit-module -o %t/Test.swiftmodule

/// Importing binary module with bridging header built from CAS from a regluar build.
/// This should succeed even it is also importing a bridging header that shares same header dependencies (with proper header guard).
// RUN: %target-swift-frontend -typecheck -module-name User -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap \
// RUN:   -I %t %t/user.swift -import-objc-header %t/Bridging2.h

/// Importing binary module with bridging header built from CAS from a cached build. This should work without additional bridging header deps.
// RUN: %target-swift-frontend -scan-dependencies -module-name User -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/user.swift -o %t/deps2.json -swift-version 5 -cache-compile-job -cas-path %t/cas \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap -I %t

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps2.json > %t/map2.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map2.json > %t/map2.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps2.json User > %t/User.cmd
// RUN: %target-swift-frontend  -cache-compile-job -module-name User -O -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -disable-implicit-swift-modules \
// RUN:   -explicit-swift-module-map-file @%t/map2.casid @%t/User.cmd %t/user.swift \
// RUN:   -emit-module -o %t/User.swiftmodule

//--- test.swift
public func test() {
    b()
}
public class TestB: B {}

//--- user.swift
import Test

func user() {
  var b: TestB
  test()
}

extension A {
    public func testA() {}
}


//--- Bridging.h
#include "Foo.h"
#include "Foo2.h"

//--- Bridging2.h
#include "Foo.h"
#include "Foo2.h"

//--- Foo.h
#import "a.h"

//--- Foo2.h
#pragma once
int Foo = 0;

//--- a.h
#include "b.h"
struct A {
  int a;
};

//--- b.h
void b(void);
@interface B
@end

//--- a.modulemap
module A {
  header "a.h"
  export *
}

//--- b.modulemap
module B {
  header "b.h"
  export *
}
