// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -o %t/deps-1.json -auto-bridging-header-chaining -cache-compile-job -cas-path %t/cas \
// RUN:   -scanner-prefix-map-paths %swift_src_root /^src -scanner-prefix-map-paths %t /^tmp \
// RUN:   -scanner-output-dir %t -import-objc-header %t/Bridging.h

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps-1.json -o %t/Test.cmd -b %t/header.cmd

// RUN: %target-swift-frontend-plain @%t/header.cmd /^tmp/Bridging.h -disable-implicit-swift-modules -O -o %t/bridging.pch
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend-plain @%t/header.cmd /^tmp/Bridging.h -disable-implicit-swift-modules -O -o %t/bridging.pch > %t/keys.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys.json > %t/key

// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/Test.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/Test.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/Test.cmd
// RUN: echo "\"/^tmp/Bridging.h\"" >> %t/Test.cmd
// RUN: echo "\"-import-pch\"" >> %t/Test.cmd
// RUN: echo "\"%t/bridging.pch\"" >> %t/Test.cmd
// RUN: echo "\"-bridging-header-pch-key\"" >> %t/Test.cmd
// RUN: echo "\"@%t/key\"" >> %t/Test.cmd
// RUN: %target-swift-frontend-plain -cache-compile-job -module-name Test -O -cas-path %t/cas @%t/Test.cmd /^tmp/test.swift \
// RUN:   -emit-module -o %t/Test.swiftmodule

// RUN: %target-swift-frontend -scan-dependencies -module-name User -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/user.swift -o %t/deps-2.json -auto-bridging-header-chaining -cache-compile-job -cas-path %t/cas \
// RUN:   -scanner-prefix-map-paths %swift_src_root /^src -scanner-prefix-map-paths %t /^tmp \
// RUN:   -scanner-output-dir %t -import-objc-header %t/Bridging.h -I %t

// RUN: %swift-scan-test -action get_chained_bridging_header -- %target-swift-frontend -scan-dependencies \
// RUN:   -module-name User -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/user.swift -o %t/deps-2.json -auto-bridging-header-chaining -cache-compile-job -cas-path %t/cas \
// RUN:   -scanner-prefix-map-paths %swift_src_root /^src -scanner-prefix-map-paths %t /^tmp \
// RUN:   -scanner-output-dir %t -import-objc-header %t/Bridging.h -I %t > %t/bridging-header.h
// RUN: %FileCheck %s --input-file=%t/bridging-header.h --check-prefix=HEADER

// HEADER: # 1 "<module-Test-embedded-bridging-header>" 1

// RUN: %{python} %S/../../utils/swift-build-modules.py --prefix-map %t /^tmp --cas %t/cas %swift_frontend_plain %t/deps-2.json -o %t/User.cmd -b %t/user-header.cmd
// RUN: %target-swift-frontend-plain @%t/user-header.cmd -disable-implicit-swift-modules -O -o %t/bridging2.pch


//--- test.swift
public func test() {
    b()
}

//--- user.swift
import Test

//--- Bridging.h
#include "Foo.h"

//--- Foo.h
void b(void);
int a = 10;
