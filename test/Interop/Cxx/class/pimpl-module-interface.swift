// RUN: %target-swift-ide-test -print-module -module-to-print=PIMPL -print-access -I %S/Inputs -cxx-interoperability-mode=default -source-filename=x | %FileCheck %s

// FIXME: We can't import std::unique_ptr properly on Windows (https://github.com/apple/swift/issues/70226)
// XFAIL: OS=windows-msvc

// CHECK:      public struct HasPIMPL {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private var ptr: OpaquePointer!
// CHECK-NEXT: }

// CHECK:      public struct HasSmartPIMPL {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private var smart_ptr: std.
// CHECK-NEXT: }
