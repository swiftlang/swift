// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule -module-name Test -O -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -import-objc-header %t/Test-Bridging.h %t/test.swift

// RUN: %target-swift-frontend -scan-dependencies -module-name User -O -I %t \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/user.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas \
// RUN:   -import-objc-header %t/User-Bridging-Bad.h -experimental-chained-bridging-header-requirement 2>&1 | %FileCheck %s --check-prefix=CHAIN-ERROR
// CHAIN-ERROR: error: bridging header doesn't chain include the bridging header

// RUN: %target-swift-frontend -scan-dependencies -module-name User -O -I %t \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/user.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas \
// RUN:   -import-objc-header %t/User-Bridging.h -experimental-chained-bridging-header-requirement 2>&1 | %FileCheck %s --check-prefix=NO-ERROR --allow-empty
// NO-ERROR-NOT: error:

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json bridgingHeader | tail -n +2  > %t/header.cmd
// RUN: %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules %t/User-Bridging.h -O -o %t/bridging.pch
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules %t/User-Bridging.h -O -o %t/bridging.pch > %t/keys.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys.json %t/User-Bridging.h > %t/key

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json User > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-swift-modules\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/bridging.pch\"" >> %t/MyApp.cmd
// RUN: echo "\"-bridging-header-pch-key\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/key\"" >> %t/MyApp.cmd
// RUN: echo "\"-explicit-swift-module-map-file\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/map.casid\"" >> %t/MyApp.cmd

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd

// RUN: %target-swift-frontend  -cache-compile-job -module-name User -O -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -disable-implicit-swift-modules \
// RUN:   -explicit-swift-module-map-file @%t/map.casid @%t/MyApp.cmd %t/user.swift \
// RUN:   -emit-module -o %t/User.swiftmodule -experimental-chained-bridging-header-requirement

//--- test.swift
public class TestA: A {}
public func test(_ : TestA) {
    foo()
}

//--- user.swift
import Test
func user() {
    var A: TestA
    foo()
    user();
}

//--- Test-Bridging.h
#include "Foo.h"

//--- User-other.h
void user(void);

//--- User-Bridging-Bad.h
#include "Foo.h"
#include "User-other.h"

//--- User-Bridging.h
#include "Test-Bridging.h"
#include "User-other.h"

//--- Foo.h
#import "a.h"

#pragma once // << pragma once only works when chaining.
void foo(void) {} // << definition imported in both bridging header.

//--- a.h
@interface A
@end

//--- a.modulemap
module A {
  header "a.h"
  export *
}
