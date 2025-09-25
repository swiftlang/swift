// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Test prefix mapped bridging header path will result in the same cache key.

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -o %t/deps-1.json -auto-bridging-header-chaining -cache-compile-job -cas-path %t/cas \
// RUN:   -scanner-prefix-map-paths %swift_src_root /^src -scanner-prefix-map-paths %t /^tmp \
// RUN:   -scanner-prefix-map-paths %t/header-1 /^header \
// RUN:   -scanner-output-dir %t/header-1 -import-objc-header %t/header-1/Bridging.h

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -o %t/deps-2.json -auto-bridging-header-chaining -cache-compile-job -cas-path %t/cas \
// RUN:   -scanner-prefix-map-paths %swift_src_root /^src -scanner-prefix-map-paths %t /^tmp \
// RUN:   -scanner-prefix-map-paths %t/header-2 /^header \
// RUN:   -scanner-output-dir %t/header-2 -import-objc-header %t/header-2/Bridging.h

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps-1.json bridgingHeader:Test includeTree > %t/includeTree-1.casid
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps-2.json bridgingHeader:Test includeTree > %t/includeTree-2.casid
// RUN: diff %t/includeTree-1.casid %t/includeTree-2.casid

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps-1.json Test casFSRootID > %t/root-1.casid
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps-2.json Test casFSRootID > %t/root-2.casid
// RUN: diff %t/root-1.casid %t/root-2.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-1.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-1.json bridgingHeader > %t/header.cmd
// RUN: %target-swift-frontend @%t/header.cmd /^header/Bridging.h -disable-implicit-swift-modules -O -o %t/bridging.pch
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend @%t/header.cmd /^header/Bridging.h -disable-implicit-swift-modules -O -o %t/bridging.pch > %t/keys.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys.json > %t/key

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-1.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-swift-modules\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/MyApp.cmd
// RUN: echo "\"/^header/Bridging.h\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-pch\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/bridging.pch\"" >> %t/MyApp.cmd
// RUN: echo "\"-bridging-header-pch-key\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/key\"" >> %t/MyApp.cmd
// RUN: echo "\"-explicit-swift-module-map-file\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/map.casid\"" >> %t/MyApp.cmd
// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps-1.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid
// RUN: %target-swift-frontend  -cache-compile-job -module-name Test -O -cas-path %t/cas @%t/MyApp.cmd /^tmp/test.swift \
// RUN:   -emit-module -o %t/Test.swiftmodule

// RUN: %target-swift-frontend -scan-dependencies -module-name User -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/user.swift -o %t/deps-3.json -auto-bridging-header-chaining -cache-compile-job -cas-path %t/cas \
// RUN:   -scanner-prefix-map-paths %swift_src_root /^src -scanner-prefix-map-paths %t /^tmp \
// RUN:   -scanner-prefix-map-paths %t/header-1 /^header \
// RUN:   -scanner-output-dir %t/header-1 -I %t

// RUN: %target-swift-frontend -scan-dependencies -module-name User -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/user.swift -o %t/deps-4.json -auto-bridging-header-chaining -cache-compile-job -cas-path %t/cas \
// RUN:   -scanner-prefix-map-paths %swift_src_root /^src -scanner-prefix-map-paths %t /^tmp \
// RUN:   -scanner-prefix-map-paths %t/header-2 /^header \
// RUN:   -scanner-output-dir %t/header-2 -I %t

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps-3.json bridgingHeader:Test includeTree > %t/includeTree-3.casid
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps-4.json bridgingHeader:Test includeTree > %t/includeTree-4.casid
// RUN: diff %t/includeTree-3.casid %t/includeTree-4.casid



//--- test.swift
public func test() {
    b()
}

//--- user.swift
import Test

//--- header-1/Bridging.h
#include "Foo.h"
//--- header-1/Foo.h
void b(void);
//--- header-2/Bridging.h
#include "Foo.h"
//--- header-2/Foo.h
void b(void);
