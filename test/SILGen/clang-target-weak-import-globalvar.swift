// REQUIRES: objc_interop
// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/deps)
// RUN: %empty-directory(%t/inputs)
// RUN: split-file %s %t

// - Prepare the explicit input map
// RUN: sed -e "s|INPUTSDIR|%/t/inputs|g" %t/map.json.template > %t/map.json.template1
// RUN: sed -e "s|STDLIBMOD|%/stdlib_module|g" %t/map.json.template1 > %t/map.json.template2
// RUN: sed -e "s|ONONEMOD|%/ononesupport_module|g" %t/map.json.template2 > %t/map.json.template3
// RUN: sed -e "s|CHEADERSDIR|%t/deps|g" %t/map.json.template3 > %t/map.json.template4
// RUN: sed -e "s|SWIFTLIBDIR|%swift-lib-dir|g" %t/map.json.template4 > %t/map.json

// - Set up explicit dependencies for the client
// RUN: %target-swift-emit-pcm -module-name SwiftShims %swift-lib-dir/swift/shims/module.modulemap -o %t/inputs/SwiftShims.pcm -target %target-cpu-apple-macosx26.0 -Xcc -Xclang -Xcc -fbuiltin-headers-in-system-modules
// RUN: %target-swift-emit-pcm -module-name Foo %t/deps/module.modulemap -o %t/inputs/Foo.pcm -target %target-cpu-apple-macosx26.0 -Xcc -Xclang -Xcc -fbuiltin-headers-in-system-modules

// - Build the client
// RUN: %target-swift-frontend -c %t/client.swift -explicit-swift-module-map-file %t/map.json -disable-implicit-swift-modules -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -o %t/client.o -target %target-cpu-apple-macosx15.5 -clang-target %target-cpu-apple-macosx26.0  -Xcc -Xclang -Xcc -fbuiltin-headers-in-system-modules

// - Ensure that the client's code-gen treats 'brandNewInteger' as a weak external symbol
// RUN: %llvm-nm -m %t/client.o | %FileCheck %s
// CHECK: (undefined) weak external _brandNewInteger

//--- deps/Foo.h
__attribute__((availability(macosx,introduced=26.0)))
__attribute__((visibility("default")))
const extern void* __nonnull brandNewInteger;

//--- deps/module.modulemap
module Foo {
    header "Foo.h"
    export *
}

//--- map.json.template
[
  {
      "moduleName": "Foo",
      "clangModulePath": "INPUTSDIR/Foo.pcm",
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


//--- client.swift
import Foo
func f() {
	if #available(macOS 26.0, *) {
		print(brandNewInteger)
	}
}
