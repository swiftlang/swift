// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %target-swift-ide-test -print-module -module-to-print=ClangIncludesModule -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s

// swift-ide-test doesn't currently typecheck the macro expansions, so run the compiler as well
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/ClangIncludes.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers %s
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/ClangIncludes.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers %s -cxx-interoperability-mode=default

import ClangIncludesModule

// CHECK:      @_exported import ModuleA
// CHECK-NEXT: @_exported import ModuleB
// CHECK-NOT:  import
// CHECK-EMPTY:

// CHECK-NEXT: func basic_include(_ p: UnsafePointer<a_t>!, _ len: a_t)
// CHECK-NEXT: func non_exported_include(_ p: UnsafePointer<b_t>!, _ len: b_t)
// CHECK-NEXT: func submodule_include(_ p: UnsafePointer<c_t>!, _ len: c_t)
// CHECK-NEXT: func explicit_submodule_include(_ p: UnsafePointer<d_t>!, _ len: d_t)
// CHECK-NEXT: func deep_submodule_noexport(_ p: UnsafePointer<e_t>!, _ len: e_t)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func basic_include(_ p: Span<a_t>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func deep_submodule_noexport(_ p: Span<e_t>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func explicit_submodule_include(_ p: Span<d_t>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func non_exported_include(_ p: Span<b_t>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func submodule_include(_ p: Span<c_t>)

public func callBasicInclude(_ p: Span<CInt>) {
  basic_include(p)
}

public func callNonExported(_ p: Span<CInt>) {
  non_exported_include(p)
}

public func callSubmoduleInclude(_ p: Span<CInt>) {
  submodule_include(p)
}

public func callExplicitSubmoduleInclude(_ p: Span<CInt>) {
  explicit_submodule_include(p)
}

public func callDeepSubmoduleNoexport(_ p: Span<CInt>) {
  deep_submodule_noexport(p)
}
