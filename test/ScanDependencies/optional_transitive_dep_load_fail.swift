// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/clang-module-cache)
// RUN: %empty-directory(%t/Foo.swiftmodule)
// RUN: %empty-directory(%t/inputs)
// RUN: echo "@_implementationOnly import A; public func foo() {}" > %t/Foo.swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

// Create the explicit module inputs map
// RUN: echo "[{" > %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"Foo\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/t/Foo.swiftmodule/%target-swiftmodule-name\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false," >> %/t/inputs/map.json
// RUN: echo "}," >> %/t/inputs/map.json
// RUN: echo "{" >> %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"Swift\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/stdlib_module\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false" >> %/t/inputs/map.json
// RUN: echo "}," >> %/t/inputs/map.json
// RUN: echo "{" >> %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"SwiftOnoneSupport\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/ononesupport_module\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false" >> %/t/inputs/map.json
// RUN: echo "}," >> %/t/inputs/map.json
// RUN: echo "{" >> %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"SwiftShims\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false," >> %/t/inputs/map.json
// RUN: echo "\"clangModuleMapPath\": \"%swift-lib-dir/swift/shims/module.modulemap\"," >> %/t/inputs/map.json
// RUN: echo "\"clangModulePath\": \"%t/inputs/SwiftShims.pcm\"" >> %/t/inputs/map.json
// RUN: echo "}]" >> %/t/inputs/map.json

// Emit the shims module PCM for explicit loading
// RUN: %target-swift-emit-pcm -module-name SwiftShims %swift-lib-dir/swift/shims/module.modulemap -o %t/inputs/SwiftShims.pcm

@testable import Foo

// Step 1: build Foo Swift module
// RUN: %target-swift-frontend -emit-module %t/Foo.swift -emit-module-path %t/Foo.swiftmodule/%target-swiftmodule-name -module-name Foo -emit-module-interface-path %t/Foo.swiftmodule/%target-swiftinterface-name -enable-library-evolution -I %S/Inputs/CHeaders -I %S/Inputs/Swift -enable-testing -swift-version 5 -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import

// Step 2: scan dependencies and ensure the transitive dependency on "A" is misssing
// RUN: %target-swift-frontend -scan-dependencies %s -o %t/deps.json -I %t -sdk %t -prebuilt-module-cache-path %t/clang-module-cache
// RUN: %validate-json %t/deps.json | %FileCheck -check-prefix CHECK_SCAN %s
// CHECK_SCAN-NOT: "swift": "A"

// Step 3: Run an explicit module compile of this file ensuring object files are produced
// RUN: %target-swift-frontend -emit-object -o %t/optional_transitive_dep_load_fail.o -disable-implicit-swift-modules -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -module-cache-path %t.module-cache -explicit-swift-module-map-file %t/inputs/map.json %s

// Step 4: Ensure the resulting object file exists
// RUN: ls %t/optional_transitive_dep_load_fail.o > /dev/null
