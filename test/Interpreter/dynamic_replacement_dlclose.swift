// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(Module1)) -DMODULE -module-name Module1 -emit-module -emit-module-path %t/Module1.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_dlclose.swift -Xfrontend -enable-private-imports
// RUN: %target-build-swift-dylib(%t/%target-library-name(Module2)) -I%t -L%t -lModule1 %target-rpath(%t) -DMODULE2 -module-name Module2 -emit-module -emit-module-path %t/Module2.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_dlclose2.swift
// RUN: %target-build-swift -I%t -L%t -lModule1 -DMAIN -o %t/main %target-rpath(%t) %s -swift-version 5
// RUN: %target-codesign %t/main %t/%target-library-name(Module1) %t/%target-library-name(Module2)
// RUN: %target-run %t/main %t/%target-library-name(Module1) %t/%target-library-name(Module2)

import Module1

import StdlibUnittest

#if os(Linux)
  import Glibc
#elseif os(Windows)
  import MSVCRT
  import WinSDK
#else
  import Darwin
#endif

var DynamicallyReplaceable = TestSuite("DynamicallyReplaceable")



private func target_library_name(_ name: String) -> String {
#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)
  return "lib\(name).dylib"
#elseif os(Windows)
  return "\(name).dll"
#else
  return "lib\(name).so"
#endif
}


DynamicallyReplaceable.test("DynamicallyReplaceable") {
  var executablePath = CommandLine.arguments[0]
  executablePath.removeLast(4)
  expectEqual(1, test())
  // Now, test with the module containing the replacements.

#if os(Linux)
	let h = dlopen(target_library_name("Module2"), RTLD_NOW)
#elseif os(Windows)
  let h = LoadLibraryA(target_library_name("Module2"))
#else
	let h = dlopen(executablePath+target_library_name("Module2"), RTLD_NOW)
#endif

  expectEqual(2, test())

#if os(Linux)
#elseif os(Windows)
#else
  dlclose(h)
#endif

#if os(Linux)
  _ = dlopen(target_library_name("Module2"), RTLD_NOW)
#elseif os(Windows)
#else
	_ = dlopen(executablePath+target_library_name("Module2"), RTLD_NOW)
#endif
  expectEqual(2, test())

}

runAllTests()
