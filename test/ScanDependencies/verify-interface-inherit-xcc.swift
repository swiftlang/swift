// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/inputs)
// RUN: %target-swift-emit-pcm -module-name SwiftShims %swift-lib-dir/swift/shims/module.modulemap -o %t/inputs/SwiftShims.pcm -Xcc -fobjc-disable-direct-methods-for-testing
// RUN: echo "[{" > %/t/inputs/map.json
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

// RUN: split-file %s %t
// RUN: %target-swift-typecheck-module-from-interface(%t/inputs/Foo.swiftinterface) -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -disable-implicit-swift-modules -explicit-swift-module-map-file %t/inputs/map.json -explicit-interface-module-build -Xcc -fobjc-disable-direct-methods-for-testing

//--- inputs/Foo.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Foo
public func foo() {}
