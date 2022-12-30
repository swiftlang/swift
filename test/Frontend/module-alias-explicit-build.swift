/// Test the -module-alias flag with an explicit module loader.

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/inputs
// RUN: mkdir -p %t/outputs

/// Create a module Bar
// RUN: echo 'public func bar() {}' > %t/inputs/FileBar.swift
// RUN: %target-swift-frontend -module-name Bar %t/inputs/FileBar.swift -emit-module -emit-module-path %t/inputs/Bar.swiftmodule

/// Check Bar.swiftmodule is created
// RUN: test -f %t/inputs/Bar.swiftmodule

/// Next create an explicit module dependency map to build module Foo
// RUN: echo 'import Cat' > %t/inputs/FileFoo.swift

// RUN: echo "[{" > %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"Bar\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/t/inputs/Bar.swiftmodule\"," >> %/t/inputs/map.json
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
// RUN: echo "}," >> %/t/inputs/map.json
// RUN: echo "{" >> %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"_Concurrency\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/concurrency_module\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false" >> %/t/inputs/map.json
// RUN: echo "}," >> %/t/inputs/map.json
// RUN: echo "{" >> %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"_StringProcessing\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/string_processing_module\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false" >> %/t/inputs/map.json
// RUN: echo "}]" >> %/t/inputs/map.json

/// Create a module Foo that imports Cat with -module-alias Cat=Bar with an explicit module loader
// RUN: %target-swift-frontend -module-name Foo %t/inputs/FileFoo.swift -module-alias Cat=Bar -I %t/inputs -emit-module -emit-module-path %t/outputs/Foo.swiftmodule -disable-implicit-swift-modules -explicit-swift-module-map-file %t/inputs/map.json -Rmodule-loading 2> %t/outputs/load-result.output

// RUN: test -f %t/outputs/Foo.swiftmodule
// RUN: test -f %t/inputs/Bar.swiftmodule
// RUN: not test -f %t/inputs/Cat.swiftmodule

// RUN: %FileCheck %s -input-file %t/outputs/load-result.output -check-prefix CHECK
// CHECK: remark: loaded module {{.*}}Bar.swiftmodule
