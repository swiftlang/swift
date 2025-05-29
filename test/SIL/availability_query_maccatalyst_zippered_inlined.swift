// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Library.swift -swift-version 5 -enable-library-evolution -module-name Library -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi -o %t -emit-module-interface-path %t/Library.swiftinterface
// RUN: %target-swift-frontend -emit-sil %t/main.swift -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi -I %t | %FileCheck %t/main.swift

// Remove the .swiftmodule and test again with the library module built from interface.

// RUN: rm %t/Library.swiftmodule
// RUN: %target-swift-frontend -emit-sil %t/main.swift -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi -I %t | %FileCheck %t/main.swift

// REQUIRES: OS=macosx || OS=maccatalyst

//--- Library.swift

public func foo() {}

@_alwaysEmitIntoClient
public func test() {
  if #available(macOS 14, iOS 17, *) {
    foo()
  }
}

//--- main.swift

import Library

test()

// CHECK-LABEL: sil shared @$s7Library4testyyF
// CHECK: [[MACOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 14
// CHECK: [[MACOS_MINOR:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[MACOS_PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[IOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 17
// CHECK: [[IOS_MINOR:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[IOS_PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[FUNC:%.*]] = function_ref @$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MACOS_MAJOR]], [[MACOS_MINOR]], [[MACOS_PATCH]], [[IOS_MAJOR]], [[IOS_MINOR]], [[IOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
