// REQUIRES: objc_interop
// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs)
// RUN: %empty-directory(%t/cheaders)

// RUN: split-file %s %t
// RUN: sed -e "s|INPUTSDIR|%/t/inputs|g" %t/map.json.template > %t/map.json.template1
// RUN: sed -e "s|STDLIBMOD|%/stdlib_module|g" %t/map.json.template1 > %t/map.json.template2
// RUN: sed -e "s|ONONEMOD|%/ononesupport_module|g" %t/map.json.template2 > %t/map.json.template3
// RUN: sed -e "s|CHEADERSDIR|%t/cheaders|g" %t/map.json.template3 > %t/map.json.template4
// RUN: sed -e "s|SWIFTLIBDIR|%swift-lib-dir|g" %t/map.json.template4 > %t/map.json

// RUN: %target-swift-emit-pcm -module-name Bar -o %t/inputs/Bar.pcm %t/cheaders/module.modulemap -target %target-cpu-apple-macosx15.0
// RUN: %target-swift-emit-pcm -module-name SwiftShims %swift-lib-dir/swift/shims/module.modulemap -o %t/inputs/SwiftShims.pcm -target %target-cpu-apple-macosx15.0

// RUN: %target-swift-frontend -c -disable-implicit-swift-modules -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -module-cache-path %t/module-cache -explicit-swift-module-map-file %t/map.json -primary-file %t/test.swift -o %t/test.o -target %target-cpu-apple-macosx14.0 -clang-target %target-cpu-apple-macosx15.0

// RUN: %llvm-nm -m %t/test.o | %FileCheck %s
// CHECK: (undefined) weak external _funcBar
// CHECK: (undefined) external _funcBarButOlder

//--- map.json.template
[
  {
      "moduleName": "Bar",
      "clangModulePath": "INPUTSDIR/Bar.pcm",
      "clangModuleMapPath": "CHEADERSDIR/module.modulemap"
  },
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

//--- cheaders/module.modulemap
module Bar {
  header "Bar.h"
  export *
}

//--- cheaders/Bar.h
#pragma clang attribute push (__attribute__((availability(macOS, introduced=15.0))), apply_to=function)
extern int funcBar(void);
#pragma clang attribute pop

#pragma clang attribute push (__attribute__((availability(macOS, introduced=14.0))), apply_to=function)
extern int funcBarButOlder(void);
#pragma clang attribute pop

//--- test.swift
import Bar
public func foo() {
    let _ = funcBarButOlder()
    if #available(macOS 15.0, *), funcBar() != 0 {
        print("Hello, World!")
    }
}
