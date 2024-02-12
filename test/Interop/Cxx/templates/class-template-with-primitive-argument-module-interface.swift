// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateWithPrimitiveArgument -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: @available(*, unavailable
// CHECK: struct MagicWrapper<T> {
// CHECK: }

// CHECK: typealias WrappedMagicInt = MagicWrapper<CInt>
// CHECK: typealias WrappedMagicIntPtr = MagicWrapper<UnsafeMutablePointer<CInt>>
// CHECK: typealias WrappedMagicIntConstPtr = MagicWrapper<UnsafePointer<CInt>>
// CHECK: typealias WrappedMagicIntPtrPtr = MagicWrapper<UnsafeMutablePointer<UnsafeMutablePointer<CInt>>>
