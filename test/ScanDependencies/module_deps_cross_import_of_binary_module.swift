// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs)
// RUN: %empty-directory(%t/binaryInputs)
// RUN: %empty-directory(%t/separateModules)
// RUN: %empty-directory(%t/inputs/Foo.swiftcrossimport)
// RUN: split-file %s %t

// - Fixup the input module file map
// RUN: sed -e "s|INPUTSDIR|%/t/inputs|g" %t/map.json.template > %t/map.json.template1
// RUN: sed -e "s|STDLIBMOD|%/stdlib_module|g" %t/map.json.template1 > %t/map.json.template2
// RUN: sed -e "s|ONONEMOD|%/ononesupport_module|g" %t/map.json.template2 > %t/map.json.template3
// RUN: sed -e "s|SWIFTLIBDIR|%swift-lib-dir|g" %t/map.json.template3 > %t/map.json

// - Pre-compile explicit module dependency inputs
// RUN: %target-swift-emit-pcm -module-name SwiftShims %swift-lib-dir/swift/shims/module.modulemap -o %t/inputs/SwiftShims.pcm

// - Pre-compile the Foo module into a separately-stored binary module
// RUN: %target-swift-frontend -compile-module-from-interface %t/separateModules/Foo.swiftinterface -o %t/binaryInputs/Foo.swiftmodule -module-name Foo -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -disable-implicit-swift-modules -Xcc -fno-implicit-modules -Xcc -fno-implicit-module-maps -explicit-swift-module-map-file %t/map.json

// - Run a dependency scan on test.swift which will pick-up the ready-made binary dependency on Foo.swiftmodule
//   and use the binary module's serialized originating defining .swiftinterface path to be able to
//   discover the cross-import overlay _Foo_Bar.
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %t/test.swift -o %t/deps.json -I %t/inputs -I %t/binaryInputs -module-name test -enable-cross-import-overlays -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import

// CHECK: "mainModuleName": "test"
// CHECK:        "swift": "test"
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT:   "modulePath": "test.swiftmodule"
// CHECK-NEXT:   "sourceFiles": [
// CHECK-NEXT:   "{{.*}}{{/|\\}}module_deps_cross_import_of_binary_module.swift.tmp{{/|\\}}test.swift"
// CHECK-NEXT:   ]
// CHECK-NEXT:   "directDependencies": [
// CHECK-DAG:      "swiftPrebuiltExternal": "Swift"
// CHECK-DAG:      "swiftPrebuiltExternal": "SwiftOnoneSupport"
// CHECK-DAG:      "swiftPrebuiltExternal": "Foo"
// CHECK-DAG:      "swift": "Bar"
// CHECK-DAG:      "swift": "_Foo_Bar"

//--- map.json.template
[
  {
      "moduleName": "Swift",
      "modulePath": "STDLIBMOD",
      "isFramework": false
  },
  {
      "moduleName": "SwiftOnoneSupport",
      "modulePath": "ONONEMOD",
      "isFramework": false
  },
  {
      "moduleName": "SwiftShims",
      "isFramework": false,
      "clangModuleMapPath": "SWIFTLIBDIR/swift/shims/module.modulemap",
      "clangModulePath": "INPUTSDIR/SwiftShims.pcm"
}]

//--- test.swift
import Foo
import Bar

//--- separateModules/Foo.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Foo
public func foo() {}

//--- inputs/Bar.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Bar
public func bar() {}

//--- inputs/_Foo_Bar.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name _Foo_Bar
public func foobar() {}

//--- separateModules/Foo.swiftcrossimport/Bar.swiftoverlay
%YAML 1.2
---
version: 1
modules:
  - name: _Foo_Bar
