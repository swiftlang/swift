// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(TestOpaque1)) -Xfrontend -enable-implicit-dynamic -Xfrontend -enable-private-imports -module-name TestOpaque1 -emit-module -emit-module-path %t/TestOpaque1.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_opaque1.swift
// RUN: %target-build-swift-dylib(%t/%target-library-name(TestOpaque2)) -I%t -L%t -lTestOpaque1 %target-rpath(%t) -module-name TestOpaque2 -swift-version 5 %S/Inputs/dynamic_replacement_opaque2.swift
// RUN: %target-build-swift -I%t -L%t -lTestOpaque1 -o %t/main %target-rpath(%t) %s -swift-version 5
// RUN: %target-codesign %t/main %t/%target-library-name(TestOpaque1) %t/%target-library-name(TestOpaque2)
// RUN: %target-run %t/main %t/%target-library-name(TestOpaque1) %t/%target-library-name(TestOpaque2) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none

// REQUIRES: CPU=arm64 || CPU=x86_64

@_private(sourceFile: "TestOpaque1.swift") import TestOpaque1

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
  import Darwin
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android) || os(Cygwin) || os(Haiku)
  import Glibc
#elseif os(Windows)
  import MSVCRT
  import WinSDK
#else
#error("Unsupported platform")
#endif

private func target_library_name(_ name: String) -> String {
#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)
  return "lib\(name).dylib"
#elseif os(Windows)
  return "\(name).dll"
#else
  return "lib\(name).so"
#endif
}

func testAssociatedType<T: Assoc> (_ t: T) {
  print(T.A.self)
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
func test() {
  print(MemoryLayout.size(ofValue: bar(5)))
  print(MemoryLayout.size(ofValue: Container().bar(5)))
  print(bar(5).myValue())
  print(Container().bar(5).myValue())
  print(MemoryLayout.size(ofValue:Container().computedProperty))
  print(Container().computedProperty.myValue())
  print(MemoryLayout.size(ofValue:Container()[0]))
  print(Container()[0].myValue())
}

// CHECK: 8
// CHECK: 8
// CHECK: 5
// CHECK: 5
// CHECK: 8
// CHECK: 2
// CHECK: 8
// CHECK: 2
if #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) {
  test()
} else {
  print("8 8 5 5 8 2 8 2")
}

var executablePath = CommandLine.arguments[0]
executablePath.removeLast(4)

#if os(Linux)
	_ = dlopen(target_library_name("TestOpaque2"), RTLD_NOW)
#elseif os(Windows)
        _ = LoadLibraryA(target_library_name("TestOpaque2"))
#else
	_ = dlopen(executablePath+target_library_name("TestOpaque2"), RTLD_NOW)
#endif

// CHECK: 16
// CHECK: 16
// CHECK: 1
// CHECK: 1
// CHECK: 16
// CHECK: 1
// CHECK: 16
// CHECK: 1
// CHECK: NewType
if #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) {
  test()
  testAssociatedType(Test())
} else {
  print("16 16 1 1 16 1 16 1 NewType")
}
