// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O -module-load-mode prefer-serialized \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -scanner-module-validation \
// RUN:   %t/test.swift -o %t/deps.json -auto-bridging-header-chaining -scanner-output-dir %t -scanner-debug-write-output \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap -import-objc-header %t/Bridging.h

// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps.json Test bridgingHeader | %FileCheck %s

// CHECK:       "moduleDependencies": [
// CHECK-NEXT:    "A"
// CHECK-NEXT:  ],

/// Try build then import from a non-caching compilation.

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:B > %t/B.cmd
// RUN: %swift_frontend_plain @%t/B.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd

// RUN: %{python} %S/../CAS/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json bridgingHeader > %t/header.cmd
// RUN: %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules -O -o %t/bridging.pch

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-swift-modules\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/Bridging.h\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-pch\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/bridging.pch\"" >> %t/MyApp.cmd
// RUN: echo "\"-explicit-swift-module-map-file\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/map.json\"" >> %t/MyApp.cmd

// RUN: %target-swift-frontend -module-name Test -O @%t/MyApp.cmd %t/test.swift \
// RUN:   -emit-module -o %t/Test.swiftmodule

/// Importing binary module with bridging header from a module that also has bridging header using implicit build method.
/// This should succeed even it is also importing a bridging header that shares same header dependencies (with proper header guard).
// RUN: %target-swift-frontend -typecheck -module-name User \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap \
// RUN:   -I %t %t/user2.swift -import-objc-header %t/Bridging2.h

/// Importing binary module with bridging header from a module that has no bridging header.
// RUN: %target-swift-frontend -scan-dependencies -module-name User -module-cache-path %t/clang-module-cache -O -module-load-mode prefer-serialized \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -scanner-module-validation \
// RUN:   %t/user.swift -o %t/deps2.json -auto-bridging-header-chaining -scanner-output-dir %t -scanner-debug-write-output \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap -I %t -enable-library-evolution

// RUN: %swift-scan-test -action get_chained_bridging_header -- %target-swift-frontend -emit-module \
// RUN:   -module-name User -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/user.swift -auto-bridging-header-chaining -o %t/User.swiftmodule \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap -I %t > %t/header1.h
// RUN:   %FileCheck %s --check-prefix=HEADER1 --input-file=%t/header1.h
// HEADER1: #include
// HEADER1-SAME: Bridging.h

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps2.json bridgingHeader > %t/header1.cmd
// RUN: %target-swift-frontend @%t/header1.cmd -disable-implicit-swift-modules -O -o %t/bridging1.pch

// RUN: %{python} %S/../CAS/Inputs/GenerateExplicitModuleMap.py %t/deps2.json > %t/map2.json
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps2.json User > %t/User.cmd
// RUN: %target-swift-frontend -module-name User -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -disable-implicit-swift-modules \
// RUN:   -import-pch %t/bridging1.pch \
// RUN:   -explicit-swift-module-map-file %t/map2.json @%t/User.cmd %t/user.swift \
// RUN:   -emit-module -o %t/User.swiftmodule -emit-module-interface-path %t/User.swiftinterface -enable-library-evolution

/// Make sure the emitted content is compatible with original. The embedded header path needs to be original header and no bridging header module leaking into interface.
// RUN: llvm-bcanalyzer -dump %t/User.swiftmodule | %FileCheck %s --check-prefix CHECK-NO-HEADER
// CHECK-NO-HEADER-NOT: <IMPORTED_HEADER
// RUN: %FileCheck %s --check-prefix NO-OBJC-LEAKING --input-file=%t/User.swiftinterface
// NO-OBJC-LEAKING-NOT: import __ObjC

/// Importing binary module with bridging header from a module with bridging header using explicit build method with header chaining.
// RUN: %target-swift-frontend -scan-dependencies -module-name User -O -module-load-mode prefer-serialized \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -scanner-module-validation \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap \
// RUN:   -I %t %t/user2.swift -import-objc-header %t/Bridging2.h -auto-bridging-header-chaining -scanner-output-dir %t -scanner-debug-write-output \
// RUN:   -o %t/deps3.json

// RUN: %swift-scan-test -action get_chained_bridging_header -- %target-swift-frontend -emit-module \
// RUN:   -module-name User -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/user.swift -auto-bridging-header-chaining -o %t/User.swiftmodule -import-objc-header %t/Bridging2.h \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap -I %t > %t/header2.h
// RUN:   %FileCheck %s --check-prefix=HEADER2 --input-file=%t/header2.h
// HEADER2: #include
// HEADER2-SAME: Bridging2.h
// HEADER2: #include
// HEADER2-SAME: Bridging.h

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps3.json clang:SwiftShims > %t/shim2.cmd
// RUN: %swift_frontend_plain @%t/shim2.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps3.json clang:B > %t/B2.cmd
// RUN: %swift_frontend_plain @%t/B2.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps3.json clang:A > %t/A2.cmd
// RUN: %swift_frontend_plain @%t/A2.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps3.json bridgingHeader > %t/header2.cmd
// RUN: %target-swift-frontend @%t/header2.cmd -disable-implicit-swift-modules -O -o %t/bridging2.pch

// RUN: %{python} %S/../CAS/Inputs/GenerateExplicitModuleMap.py %t/deps3.json > %t/map3.json
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps3.json User > %t/User2.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/User2.cmd
// RUN: echo "\"%t/Bridging2.h\"" >> %t/User2.cmd
// RUN: echo "\"-import-pch\"" >> %t/User2.cmd
// RUN: echo "\"%t/bridging2.pch\"" >> %t/User2.cmd

// RUN: %target-swift-frontend -module-name User -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -disable-implicit-swift-modules \
// RUN:   -explicit-swift-module-map-file %t/map3.json @%t/User2.cmd %t/user2.swift \
// RUN:   -emit-objc-header -emit-objc-header-path %t/User-Swift.h \
// RUN:   -emit-module -o %t/User2.swiftmodule

/// Generated ObjC Header should reference original header name, not the chained bridging header.
// RUN: %FileCheck %s --check-prefix OBJC-HEADER --input-file=%t/User-Swift.h
// OBJC-HEADER: import B
// OBJC-HEADER: import
// OBJC-HEADER-SAME: Bridging2.h
// OBJC-HEADER-NOT: ChainedBridgingHeader.h

// RUN: %target-swift-frontend -scan-dependencies -module-name User -O -module-load-mode prefer-serialized \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -scanner-module-validation \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap \
// RUN:   -I %t %t/user2.swift -import-objc-header %t/Bridging3.h -auto-bridging-header-chaining -scanner-output-dir %t -scanner-debug-write-output \
// RUN:   -o %t/deps4.json -Rdependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache

/// Make sure the cache is correct.
// RUN: %target-swift-frontend -scan-dependencies -module-name User -O -module-load-mode prefer-serialized \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -scanner-module-validation \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap \
// RUN:   -I %t %t/user2.swift -import-objc-header %t/Bridging3.h -auto-bridging-header-chaining -scanner-output-dir %t -scanner-debug-write-output \
// RUN:   -o %t/deps4.json -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache \
// RUN:   -dependency-scan-cache-path %t/cache.moddepcache 2>&1 | %FileCheck %s -check-prefix=CACHE-LOAD
// CACHE-LOAD: remark: Incremental module scan: Re-using serialized module scanning dependency cache from:

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps4.json bridgingHeader > %t/header3.cmd
// RUN: %target-swift-frontend @%t/header3.cmd -disable-implicit-swift-modules -O -o %t/bridging3.pch

// RUN: %{python} %S/../CAS/Inputs/GenerateExplicitModuleMap.py %t/deps4.json > %t/map4.json
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps4.json User > %t/User3.cmd
// RUN: echo "\"-import-pch\"" >> %t/User3.cmd
// RUN: echo "\"%t/bridging3.pch\"" >> %t/User3.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/User3.cmd
// RUN: echo "\"%t/Bridging3.h\"" >> %t/User3.cmd

// RUN: %target-swift-frontend -module-name User -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -disable-implicit-swift-modules \
// RUN:   -explicit-swift-module-map-file %t/map4.json @%t/User3.cmd %t/user2.swift \
// RUN:   -emit-module -o %t/User3.swiftmodule

/// Verify the encoded here is just the `-import-objc-header` option.
// RUN: llvm-bcanalyzer -dump %t/User3.swiftmodule | %FileCheck %s --check-prefix CHECK-HEADER
// CHECK-HEADER: <IMPORTED_HEADER
// CHECK-HEADER-SAME: Bridging3.h

/// TODO: Try removing the bridging header and using the embedded header.
/// This is currently not available for testing due to a bug in embedded header
/// generation when explicit module is on. rdar://142505969

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

public class AB : B {}

//--- user2.swift
import Test

func user() {
  var b: TestB
  var bridging: Bridging2
  test()
}

extension Bridging2 {
    public func testA() {}
}

public class BB : B {}
public class Foo3 : Bridging2 {}

//--- Bridging.h
#include "Foo.h"
#include "Foo2.h"

//--- Bridging2.h
#include "Foo.h"
#include "Foo2.h"
@interface Bridging2
@end

//--- Bridging3.h
@interface Bridging2
@end

//--- Foo.h
#import "a.h"
#ifndef IMPORT_FOO
#define IMPORT_FOO
int Foo = 0;
#endif

//--- Foo2.h
#ifndef IMPORT_FOO2
#define IMPORT_FOO2
int Foo2 = 0;
#endif

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
