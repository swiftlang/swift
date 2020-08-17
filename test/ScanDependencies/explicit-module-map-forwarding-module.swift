// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/inputs
// RUN: echo "/// Some cool comments" > %t/foo.swift
// RUN: echo "public func foo() {}" >> %t/foo.swift

// Step 1: build .swiftmodule and .swiftinterface adjacent to each other from foo.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/inputs/Foo.swiftmodule -emit-module-doc-path %t/inputs/Foo.swiftdoc -emit-module-source-info -emit-module-source-info-path %t/inputs/Foo.swiftsourceinfo -emit-module-interface-path %t/inputs/Foo.swiftinterface -module-cache-path %t.module-cache %t/foo.swift -module-name Foo

// Step 2: build .swiftmodule from .swiftinterface and give the adjacent .swiftmodule as a candidate compiled module.
// RUN: %target-swift-frontend -compile-module-from-interface %t/inputs/Foo.swiftinterface -o %t/inputs/Foo-from-interface.swiftmodule -module-name Foo -candidate-module-file %t/inputs/Foo.swiftmodule

// Step 3: the new .swiftmodule should be a fowarding module.
// RUN: %{python} %S/../ModuleInterface/ModuleCache/Inputs/check-is-forwarding-module.py %t/inputs/Foo-from-interface.swiftmodule

// Step 4: using the forwarding module in explicit module map should be OK.
// RUN: echo "[{" > %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"Foo\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/t/inputs/Foo-from-interface.swiftmodule\"," >> %/t/inputs/map.json
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

// RUN: %target-swift-ide-test -print-module-comments -module-to-print=Foo -enable-swiftsourceinfo -source-filename %s -explicit-swift-module-map-file %t/inputs/map.json | %FileCheck %s

// CHECK: foo.swift:2:13: Func/foo RawComment=[/// Some cool comments
