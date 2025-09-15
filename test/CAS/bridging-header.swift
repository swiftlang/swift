// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -o %t/deps.json -auto-bridging-header-chaining -cache-compile-job -cas-path %t/cas \
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

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json bridgingHeader > %t/header.cmd
// RUN: %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules -O -o %t/bridging.pch
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules -O -o %t/bridging.pch > %t/keys.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys.json > %t/key

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-swift-modules\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/Bridging.h\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-pch\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/bridging.pch\"" >> %t/MyApp.cmd
// RUN: echo "\"-bridging-header-pch-key\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/key\"" >> %t/MyApp.cmd
// RUN: echo "\"-explicit-swift-module-map-file\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/map.casid\"" >> %t/MyApp.cmd

// RUN: %target-swift-frontend  -cache-compile-job -module-name Test -O -cas-path %t/cas @%t/MyApp.cmd %t/test.swift \
// RUN:   -emit-module -o %t/Test.swiftmodule

/// Importing binary module with bridging header built from CAS from a regluar build.
/// This should succeed even it is also importing a bridging header that shares same header dependencies (with proper header guard).
// RUN: %target-swift-frontend -typecheck -module-name User -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap \
// RUN:   -I %t %t/user2.swift -import-objc-header %t/Bridging2.h

/// Importing binary module with bridging header built from CAS from a cached build. This should work without additional bridging header deps.
// RUN: %target-swift-frontend -scan-dependencies -module-name User -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/user.swift -o %t/deps2.json -auto-bridging-header-chaining -cache-compile-job -cas-path %t/cas \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap -I %t

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps2.json User chainedBridgingHeaderPath | %FileCheck %s --check-prefix HEADER-PATH
// HEADER-PATH: <compiler-generated>{{/|\\}}User-{{.*}}-ChainedBridgingHeader.h

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps2.json bridgingHeader > %t/header1.cmd
// RUN: %target-swift-frontend @%t/header1.cmd -disable-implicit-swift-modules -O -o %t/bridging1.pch

// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend @%t/header1.cmd -disable-implicit-swift-modules -O -o %t/bridging1.pch > %t/keys1.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys1.json > %t/key1

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps2.json > %t/map2.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map2.json > %t/map2.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps2.json User > %t/User.cmd
// RUN: %target-swift-frontend  -cache-compile-job -module-name User -O -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -disable-implicit-swift-modules \
// RUN:   -import-pch %t/bridging1.pch -bridging-header-pch-key @%t/key1 \
// RUN:   -explicit-swift-module-map-file @%t/map2.casid @%t/User.cmd %t/user.swift \
// RUN:   -emit-module -o %t/User.swiftmodule

// RUN: llvm-bcanalyzer -dump %t/User.swiftmodule | %FileCheck %s --check-prefix CHECK-NO-HEADER
// CHECK-NO-HEADER-NOT: <IMPORTED_HEADER

/// Importing binary module with bridging header for a cached build while also importing a bridging header.
// RUN: %target-swift-frontend -scan-dependencies -module-name User -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap \
// RUN:   -I %t %t/user2.swift -import-objc-header %t/Bridging2.h -cache-compile-job -cas-path %t/cas \
// RUN:   -o %t/deps3.json -auto-bridging-header-chaining
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps3.json clang:SwiftShims > %t/shim2.cmd
// RUN: %swift_frontend_plain @%t/shim2.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps3.json clang:B > %t/B2.cmd
// RUN: %swift_frontend_plain @%t/B2.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps3.json clang:A > %t/A2.cmd
// RUN: %swift_frontend_plain @%t/A2.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps3.json bridgingHeader > %t/header2.cmd
// RUN: %target-swift-frontend @%t/header2.cmd -disable-implicit-swift-modules -O -o %t/bridging2.pch

// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend @%t/header2.cmd -disable-implicit-swift-modules -O -o %t/bridging2.pch > %t/keys2.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys2.json > %t/key2

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps3.json > %t/map3.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map3.json > %t/map3.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps3.json User > %t/User2.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/User2.cmd
// RUN: echo "\"%t/Bridging2.h\"" >> %t/User2.cmd
// RUN: echo "\"-import-pch\"" >> %t/User2.cmd
// RUN: echo "\"%t/bridging2.pch\"" >> %t/User2.cmd
// RUN: echo "\"-bridging-header-pch-key\"" >> %t/User2.cmd
// RUN: echo "\"@%t/key2\"" >> %t/User2.cmd

// RUN: %target-swift-frontend  -cache-compile-job -module-name User -O -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -disable-implicit-swift-modules \
// RUN:   -explicit-swift-module-map-file @%t/map3.casid @%t/User2.cmd %t/user2.swift \
// RUN:   -emit-module -o %t/User2.swiftmodule

// RUN: %target-swift-frontend -scan-dependencies -module-name User -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap \
// RUN:   -I %t %t/user2.swift -import-objc-header %t/Bridging3.h -cache-compile-job -cas-path %t/cas \
// RUN:   -o %t/deps4.json -auto-bridging-header-chaining
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps4.json bridgingHeader > %t/header3.cmd
// RUN: %target-swift-frontend @%t/header3.cmd -disable-implicit-swift-modules -O -o %t/bridging3.pch

// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend @%t/header3.cmd -disable-implicit-swift-modules -O -o %t/bridging3.pch > %t/keys3.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys3.json > %t/key3

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps4.json > %t/map4.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map4.json > %t/map4.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps4.json User > %t/User3.cmd
// RUN: echo "\"-import-pch\"" >> %t/User3.cmd
// RUN: echo "\"%t/bridging3.pch\"" >> %t/User3.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/User3.cmd
// RUN: echo "\"%t/Bridging3.h\"" >> %t/User3.cmd
// RUN: echo "\"-bridging-header-pch-key\"" >> %t/User3.cmd
// RUN: echo "\"@%t/key3\"" >> %t/User3.cmd

// RUN: %target-swift-frontend  -cache-compile-job -module-name User -O -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -disable-implicit-swift-modules \
// RUN:   -explicit-swift-module-map-file @%t/map4.casid @%t/User3.cmd %t/user2.swift \
// RUN:   -emit-module -o %t/User3.swiftmodule

/// Verify the encoded here is just the `-import-objc-header` option.
// RUN: llvm-bcanalyzer -dump %t/User3.swiftmodule | %FileCheck %s --check-prefix CHECK-HEADER
// CHECK-HEADER: <IMPORTED_HEADER
// CHECK-HEADER-SAME: Bridging3.h

/// Try removing the bridging header and using the embedded header.
// RUN: rm %t/Bridging.h
// RUN: %target-swift-frontend -scan-dependencies -module-name User -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap \
// RUN:   -I %t %t/user2.swift -import-objc-header %t/Bridging3.h -cache-compile-job -cas-path %t/cas \
// RUN:   -o %t/deps5.json -auto-bridging-header-chaining
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps5.json bridgingHeader > %t/header4.cmd
// RUN: %target-swift-frontend @%t/header4.cmd -disable-implicit-swift-modules -O -o %t/bridging4.pch

// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend @%t/header4.cmd -disable-implicit-swift-modules -O -o %t/bridging4.pch > %t/keys4.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys4.json > %t/key4

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps5.json > %t/map5.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map5.json > %t/map5.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps5.json User > %t/User4.cmd
// RUN: echo "\"-import-pch\"" >> %t/User4.cmd
// RUN: echo "\"%t/bridging4.pch\"" >> %t/User4.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/User4.cmd
// RUN: echo "\"%t/Bridging4.h\"" >> %t/User4.cmd
// RUN: echo "\"-bridging-header-pch-key\"" >> %t/User4.cmd
// RUN: echo "\"@%t/key4\"" >> %t/User4.cmd

// RUN: %target-swift-frontend  -cache-compile-job -module-name User -O -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -disable-implicit-swift-modules \
// RUN:   -explicit-swift-module-map-file @%t/map5.casid @%t/User4.cmd %t/user2.swift \
// RUN:   -emit-module -o %t/User5.swiftmodule

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
