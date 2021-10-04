/// Test the -module-alias flag with an explicit module loader.

// RUN: %empty-directory(%t)

/// Create a module Bar
// RUN: echo 'public func bar() {}' > %t/FileBar.swift
// RUN: %target-swift-frontend -module-name Bar %t/FileBar.swift -emit-module -emit-module-path %t/Bar.swiftmodule

/// Check Bar.swiftmodule is created
// RUN: test -f %t/Bar.swiftmodule

/// Next create an explicit module dependency map to build module Foo
// RUN: mkdir -p %t/inputs
// RUN: echo 'import Cat' > %t/FileFoo.swift

// RUN: echo "[{" > %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"Foo\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/t/inputs/Foo.swiftmodule\"," >> %/t/inputs/map.json
// RUN: echo "\"docPath\": \"%/t/inputs/Foo.swiftdoc\"," >> %/t/inputs/map.json
// RUN: echo "\"sourceInfoPath\": \"%/t/inputs/Foo.swiftsourceinfo\"," >> %/t/inputs/map.json
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
// RUN: echo "\"moduleName\": \"Swift\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/stdlib_module\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false" >> %/t/inputs/map.json
// RUN: echo "}," >> %/t/inputs/map.json
// RUN: echo "{" >> %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"_Distributed\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/distributed_module\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false" >> %/t/inputs/map.json
// RUN: echo "}]" >> %/t/inputs/map.json

/// Create a module Foo that imports Cat with -module-alias Cat=Bar with an explicit module loader
// RUN: %target-swift-frontend -module-name Foo %t/FileFoo.swift -module-alias Cat=Bar -I %t -emit-module -emit-module-path %t/Foo.swiftmodule -disable-implicit-swift-modules -explicit-swift-module-map-file %t/inputs/map.json -Rmodule-loading 2> %t/load-result.output

// RUN: test -f %t/Foo.swiftmodule
// RUN: test -f %t/Bar.swiftmodule
// RUN: not test -f %t/Cat.swiftmodule

// RUN: %FileCheck %s -input-file %t/load-result.output -check-prefix CHECK
// CHECK: remark: loaded module at {{.*}}Bar.swiftmodule

