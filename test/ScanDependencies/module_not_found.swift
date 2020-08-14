// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/inputs
// RUN: echo "public func foo() {}" >> %t/foo.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/inputs/Foo.swiftmodule -emit-module-doc-path %t/inputs/Foo.swiftdoc -emit-module-source-info -emit-module-source-info-path %t/inputs/Foo.swiftsourceinfo -module-cache-path %t.module-cache %t/foo.swift -module-name Foo

// RUN: echo "[{" >> %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"Swift\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%stdlib_dir/Swift.swiftmodule/%module-target-triple.swiftmodule\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false" >> %/t/inputs/map.json
// RUN: echo "}," >> %/t/inputs/map.json
// RUN: echo "{" >> %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"SwiftOnoneSupport\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%stdlib_dir/SwiftOnoneSupport.swiftmodule/%module-target-triple.swiftmodule\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false" >> %/t/inputs/map.json
// RUN: echo "}]" >> %/t/inputs/map.json

// Add the -I search path to ensure we do not accidentally implicitly load Foo.swiftmodule
// RUN: not %target-swift-frontend -typecheck %s -I %t/inputs -explicit-swift-module-map-file %t/inputs/map.json -disable-implicit-swift-modules
import Foo
// CHECK: error: no such module 'Foo'

