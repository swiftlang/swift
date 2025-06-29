// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %target-swift-ide-test -print-module -module-to-print=VsnprintfClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers -Xcc -Wno-nullability-completeness | %FileCheck %s

// swift-ide-test doesn't currently typecheck the macro expansions, so run the compiler as well
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/Vsnprintf.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness %s

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macro for vsnprintf with __counted_by and va_list parameters.

import VsnprintfClang

// CHECK:      /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func vsnprintf(_ __str: UnsafeMutableBufferPointer<CChar>, _ __format: UnsafePointer<CChar>!, _ foo_args: CVaListPointer) -> Int32

@inlinable
public func callVsnprintf(_ p: UnsafeMutableBufferPointer<CChar>) {
    unsafe withVaList([CLongLong(42), CInt(1337)]) { args in
        _ = unsafe vsnprintf(p.baseAddress!, p.count, "%lld %d", args)
    }
}
