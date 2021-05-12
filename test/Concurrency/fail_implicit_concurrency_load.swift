// This test ensures that if implicit import of the Concurrency module is enabled,
// but no such module can be located (here verified by forcing explicit modules),
// a warning diagnostic is emitted.
// REQUIRES: concurrency
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/inputs

// RUN: echo "[{" > %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"Swift\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/stdlib_module\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false" >> %/t/inputs/map.json
// RUN: echo "}," >> %/t/inputs/map.json
// RUN: echo "{" >> %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"SwiftOnoneSupport\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/ononesupport_module\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false" >> %/t/inputs/map.json
// RUN: echo "}]" >> %/t/inputs/map.json

// RUN: %target-swift-frontend -typecheck %s -explicit-swift-module-map-file %t/inputs/map.json -disable-implicit-swift-modules -enable-experimental-concurrency 2>&1 | %FileCheck %s
import Swift
// CHECK: warning: unable to perform implicit import of "_Concurrency" module: no such module found
