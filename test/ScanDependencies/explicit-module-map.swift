// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/inputs
// RUN: echo "/// Some cool comments" > %t/foo.swift
// RUN: echo "public func foo() {}" >> %t/foo.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/inputs/Foo.swiftmodule -emit-module-doc-path %t/inputs/Foo.swiftdoc -emit-module-source-info -emit-module-source-info-path %t/inputs/Foo.swiftsourceinfo -module-cache-path %t.module-cache %t/foo.swift -module-name Foo

// RUN: echo "[{" > %/t/inputs/map.json
// RUN: echo "\"SwiftModule\": \"Foo\"," >> %/t/inputs/map.json
// RUN: echo "\"SwiftModulePath\": \"%/t/inputs/Foo.swiftmodule\"," >> %/t/inputs/map.json
// RUN: echo "\"SwiftDocPath\": \"%/t/inputs/Foo.swiftdoc\"," >> %/t/inputs/map.json
// RUN: echo "\"SwiftSourceInfoPath\": \"%/t/inputs/Foo.swiftsourceinfo\"" >> %/t/inputs/map.json
// RUN: echo "}]" >> %/t/inputs/map.json

// RUN: %target-swift-ide-test -print-module-comments -module-to-print=Foo -enable-swiftsourceinfo -source-filename %s -explicit-swift-module-map-file %t/inputs/map.json | %FileCheck %s

// CHECK: foo.swift:2:13: Func/foo RawComment=[/// Some cool comments
