// First build without chaining.
// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/libA.%target-dylib-extension) -module-name A -emit-module -emit-module-path %t -swift-version 5 %S/Inputs/dynamic_replacement_chaining_A.swift
// RUN: %target-build-swift-dylib(%t/libB.%target-dylib-extension) -I%t -L%t -lA -Xlinker -rpath -Xlinker %t -module-name B -emit-module -emit-module-path %t -swift-version 5 %S/Inputs/dynamic_replacement_chaining_B.swift
// RUN: %target-build-swift-dylib(%t/libC.%target-dylib-extension) -I%t -L%t -lA -Xlinker -rpath -Xlinker %t -module-name C -emit-module -emit-module-path %t -swift-version 5 %S/Inputs/dynamic_replacement_chaining_B.swift
// RUN: %target-build-swift -I%t -L%t -lA -o %t/main -Xlinker -rpath -Xlinker %t %s -swift-version 5
// RUN: %target-codesign %t/main %t/libA.%target-dylib-extension %t/libB.%target-dylib-extension %t/libC.%target-dylib-extension
// RUN: %target-run %t/main %t/libA.%target-dylib-extension %t/libB.%target-dylib-extension %t/libC.%target-dylib-extension

// Now build with chaining enabled.
// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/libA.%target-dylib-extension) -module-name A -emit-module -emit-module-path %t -swift-version 5 %S/Inputs/dynamic_replacement_chaining_A.swift
// RUN: %target-build-swift-dylib(%t/libB.%target-dylib-extension) -I%t -L%t -lA -Xlinker -rpath -Xlinker %t -module-name B -emit-module -emit-module-path %t -swift-version 5 %S/Inputs/dynamic_replacement_chaining_B.swift -Xfrontend -enable-dynamic-replacement-chaining
// RUN: %target-build-swift-dylib(%t/libC.%target-dylib-extension) -I%t -L%t -lA -Xlinker -rpath -Xlinker %t -module-name C -emit-module -emit-module-path %t -swift-version 5 %S/Inputs/dynamic_replacement_chaining_B.swift -Xfrontend -enable-dynamic-replacement-chaining
// RUN: %target-build-swift -I%t -L%t -lA -DCHAINING -o %t/main -Xlinker -rpath -Xlinker %t %s -swift-version 5
// RUN: %target-codesign %t/main %t/libA.%target-dylib-extension %t/libB.%target-dylib-extension %t/libC.%target-dylib-extension
// RUN: %target-run %t/main %t/libA.%target-dylib-extension %t/libB.%target-dylib-extension %t/libC.%target-dylib-extension

// REQUIRES: executable_test

import A

import StdlibUnittest

#if os(Linux)
  import Glibc
  let dylibSuffix = "so"
#else
  import Darwin
  let dylibSuffix = "dylib"
#endif

var DynamicallyReplaceable = TestSuite("DynamicallyReplaceableChaining")


DynamicallyReplaceable.test("DynamicallyReplaceable") {
  var executablePath = CommandLine.arguments[0]
  executablePath.removeLast(4)

#if os(Linux)
	_ = dlopen("libB."+dylibSuffix, RTLD_NOW)
	_ = dlopen("libC."+dylibSuffix, RTLD_NOW)
#else
	_ = dlopen(executablePath+"libB."+dylibSuffix, RTLD_NOW)
	_ = dlopen(executablePath+"libC."+dylibSuffix, RTLD_NOW)
#endif

#if CHAINING
  expectEqual(3, Impl().foo())
#else
  expectEqual(2, Impl().foo())
#endif
}

runAllTests()
