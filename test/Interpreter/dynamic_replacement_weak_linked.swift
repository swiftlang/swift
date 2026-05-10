// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(LibA)) -DBEFORE -module-name LibA -emit-module -emit-module-path %t/LibA.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_libA.swift
// RUN: %target-build-swift-dylib(%t/%target-library-name(ReplacementA)) -I%t -L%t -lLibA -module-name ReplacementA -swift-version 5 %S/Inputs/dynamic_replacement_ReplacementA.swift
// RUN: %target-build-swift-dylib(%t/%target-library-name(LibA)) -module-name LibA -emit-module -emit-module-path %t/LibA.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_libA.swift
// RUN: %target-build-swift -I%t -L%t -lLibA -o %t/main %target-rpath(%t) %s -swift-version 5
// RUN: %target-codesign %t/main %t/%target-library-name(LibA) %t/%target-library-name(ReplacementA)
// RUN: %target-run %t/main %t/%target-library-name(LibA) %t/%target-library-name(ReplacementA)

// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: swift_test_mode_optimize_none

import Darwin

import LibA

private func target_library_name(_ name: String) -> String {
  return "lib\(name).dylib"
}

var executablePath = CommandLine.arguments[0]
executablePath.removeLast(4)
_ = dlopen(executablePath + target_library_name("ReplacementA"), RTLD_NOW)

A().print()
