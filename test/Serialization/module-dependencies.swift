// RUN: %empty-directory(%t)
// RUN: mkdir %t/inputs
// RUN: echo "[{" > %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"A\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/t/inputs/A.swiftmodule\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false" >> %/t/inputs/map.json
// RUN: echo "}," >> %/t/inputs/map.json
// RUN: echo "{" >> %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"B\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/t/inputs/B.swiftmodule\"," >> %/t/inputs/map.json
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
// RUN: echo "]" >> %/t/inputs/map.json
// RUN: %target-swift-frontend -emit-module -o %t/inputs/B.swiftmodule %S/../Inputs/empty.swift -disable-implicit-swift-modules -parse-stdlib
//-no-serialize-debugging-options
// RUN: %target-swift-frontend -emit-module -o %t/inputs/A.swiftmodule %S/Inputs/a.swift -swift-module-file=B=%t/inputs/B.swiftmodule -disable-implicit-swift-modules -parse-stdlib

// RUN: %target-swift-frontend -emit-module -o %t/Library.swiftmodule %s -disable-implicit-swift-modules -parse-stdlib -explicit-swift-module-map-file %t/inputs/map.json -g
// RUN: %lldb-moduleimport-test %t/Library.swiftmodule --dump-explicit-module-map | %FileCheck %s

// Test the the explicit module map got serialized.

// CHECK-DAG: inputs{{/|\\\\}}A.swiftmodule
// CHECK-DAG: inputs{{/|\\\\}}B.swiftmodule
// CHECK-DAG: Swift.swiftmodule
import A
