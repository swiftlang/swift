// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/inputs
// RUN: mkdir -p %t/barinputs
// RUN: echo "public func foo() {}" >> %t/foo.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/inputs/Foo.swiftmodule -emit-module-doc-path %t/inputs/Foo.swiftdoc -emit-module-source-info -emit-module-source-info-path %t/inputs/Foo.swiftsourceinfo -module-cache-path %t.module-cache %t/foo.swift -module-name Foo -user-module-version 9001
// RUN: echo "public func bar() {}" >> %t/bar.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/barinputs/Bar.swiftmodule -emit-module-doc-path %t/barinputs/Bar.swiftdoc -emit-module-source-info -emit-module-source-info-path %t/barinputs/Bar.swiftsourceinfo -module-cache-path %t.module-cache %t/bar.swift -module-name Bar

// RUN: echo "---"                                  >> %t/inputs/ForwardingFoo.swiftmodule
// RUN: echo "path: '%/t/inputs/Foo.swiftmodule'" >> %t/inputs/ForwardingFoo.swiftmodule
// RUN: echo "dependencies:    []"                  >> %t/inputs/ForwardingFoo.swiftmodule
// RUN: echo "version:         1 "                  >> %t/inputs/ForwardingFoo.swiftmodule
// RUN: echo "..."                                  >> %t/inputs/ForwardingFoo.swiftmodule

// RUN: echo "[{" > %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"Foo\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/t/inputs/ForwardingFoo.swiftmodule\"," >> %/t/inputs/map.json
// RUN: echo "\"docPath\": \"%/t/inputs/Foo.swiftdoc\"," >> %/t/inputs/map.json
// RUN: echo "\"sourceInfoPath\": \"%/t/inputs/Foo.swiftsourceinfo\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false" >> %/t/inputs/map.json
// RUN: echo "}]" >> %/t/inputs/map.json

// RUN: not %target-swift-frontend -typecheck %s -explicit-swift-module-map-file %t/inputs/map.json -disable-implicit-swift-modules
// RUN: %target-swift-frontend -typecheck %s -explicit-swift-module-map-file %t/inputs/map.json -I %t/barinputs -Rmodule-loading 2>&1 | %FileCheck -check-prefix=POSITIVE-CHECK %s

#if canImport(Foo, _version: 9000)
import Bar
#endif

// 'Bar' can only be imported if the explicitly-loaded 'Foo' is known to be over 9000
// POSITIVE-CHECK: remark: loaded module 'Bar'

// Re-build Foo with a lower version and ensure we do not import `Bar` 
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/inputs/Foo.swiftmodule -emit-module-doc-path %t/inputs/Foo.swiftdoc -emit-module-source-info -emit-module-source-info-path %t/inputs/Foo.swiftsourceinfo -module-cache-path %t.module-cache %t/foo.swift -module-name Foo -user-module-version 7000
// RUN: %target-swift-frontend -typecheck %s -explicit-swift-module-map-file %t/inputs/map.json -I %t/barinputs -Rmodule-loading 2>&1 | %FileCheck -check-prefix=NEGATIVE-CHECK %s

// 'Bar' can only be imported if the explicitly-loaded 'Foo' is known to be over 9000
// NEGATIVE-CHECK-NOT: remark: loaded module 'Bar'
