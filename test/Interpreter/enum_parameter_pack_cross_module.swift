// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(Lib)) -target %target-swift-5.9-abi-triple -enable-library-evolution %S/Inputs/enum_parameter_pack_lib.swift -emit-module -emit-module-path %t/Lib.swiftmodule -module-name Lib
// RUN: %target-codesign %t/%target-library-name(Lib)
// RUN: %target-build-swift %s -target %target-swift-5.9-abi-triple -lLib -I %t -L %t -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(Lib)

// REQUIRES: executable_test
// REQUIRES: objc_interop

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// This test validates that cross-module instantiation of generic enums with
// parameter packs works correctly when the enum contains nested structs that
// reference types from external frameworks like Foundation.
//
// The bug (now fixed) was that pack metadata pointers were incorrectly passed
// to swift_checkMetadataState during metadata completion, causing a crash
// because pack pointers are arrays of metadata pointers, not actual metadata.

import Lib

let actions: [Action<Int>] = []
print("Empty array:", actions)

let moreActions: [Action<Int, String>] = []
print("Two-element pack array:", moreActions)

print("Success - no crash")
