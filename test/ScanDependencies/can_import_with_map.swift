// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/inputs
// RUN: echo "public func foo() {}" >> %t/foo.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/inputs/Foo.swiftmodule -emit-module-doc-path %t/inputs/Foo.swiftdoc -emit-module-source-info -emit-module-source-info-path %t/inputs/Foo.swiftsourceinfo -module-cache-path %t.module-cache %t/foo.swift -module-name Foo

// RUN: echo "[{" > %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"Foo\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/t/inputs/Foo.swiftmodule\"," >> %/t/inputs/map.json
// RUN: echo "\"docPath\": \"%/t/inputs/Foo.swiftdoc\"," >> %/t/inputs/map.json
// RUN: echo "\"sourceInfoPath\": \"%/t/inputs/Foo.swiftsourceinfo\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false" >> %/t/inputs/map.json
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
// RUN: echo "}]" >> %/t/inputs/map.json

// RUN: %target-swift-frontend -typecheck %s -explicit-swift-module-map-file %t/inputs/map.json -disable-implicit-swift-modules
#if canImport(Foo)
import Foo
#endif
