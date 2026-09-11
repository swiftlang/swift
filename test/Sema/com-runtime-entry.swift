// RUN: %empty-directory(%t/valid)
// RUN: %empty-directory(%t/invalid)
// RUN: %empty-directory(%t/invalid-types)
// RUN: %empty-directory(%t/ambiguous)
// RUN: %empty-directory(%t/aggregated)
// RUN: %empty-directory(%t/missing-defaults)
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -module-name COM -emit-module-path %t/valid/COM.swiftmodule %S/Inputs/com-runtime-entry.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -module-name COM -emit-module-path %t/invalid/COM.swiftmodule -D INVALID_QUERY_INTERFACE -D MISSING_RELEASE %S/Inputs/com-runtime-entry.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -module-name COM -emit-module-path %t/invalid-types/COM.swiftmodule -D INVALID_RUNTIME_ENTRY_TYPES %S/Inputs/com-runtime-entry.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -module-name COM -emit-module-path %t/ambiguous/COM.swiftmodule -D AMBIGUOUS_QUERY_INTERFACE %S/Inputs/com-runtime-entry.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -module-name COM -emit-module-path %t/aggregated/COM.swiftmodule %S/Inputs/com-runtime-entry.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -module-name COM -emit-module-path %t/missing-defaults/COM.swiftmodule -D MISSING_ISWIFTOBJECT_DEFAULTS %S/Inputs/com-runtime-entry.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -I %t/valid -typecheck %s
// RUN: not %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -I %t/invalid -typecheck %s 2>&1 | %FileCheck %s --check-prefix=INVALID
// RUN: not %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -I %t/invalid-types -typecheck %s 2>&1 | %FileCheck %s --check-prefix=INVALID-TYPES
// RUN: not %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -I %t/ambiguous -typecheck %s 2>&1 | %FileCheck %s --check-prefix=AMBIGUOUS
// RUN: not %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -I %t/aggregated -D AGGREGATED -typecheck %s 2>&1 | %FileCheck %s --check-prefix=AGGREGATED
// RUN: not %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -I %t/missing-defaults -typecheck %s 2>&1 | %FileCheck %s --check-prefix=MISSING-DEFAULTS

import COM

#if AGGREGATED
@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IWidget: IUnknown { }
#else
@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IWidget { }
#endif

#if AGGREGATED
@com
final class Widget: IWidget, COMAggregatable {
  let controller: (any IUnknown)? = nil
}

@com
final class OtherWidget: IWidget, COMAggregatable {
  let controller: (any IUnknown)? = nil
}
#else
@com
final class Widget: IWidget { }

@com
final class OtherWidget: IWidget { }
#endif

// INVALID-COUNT-1: error: function 'QueryInterface' in the 'COM' module must provide a direct C entry point
// INVALID-COUNT-1: error: function 'Release' not found in the 'COM' module
// INVALID-TYPES-COUNT-1: error: function 'QueryInterface' in the 'COM' module has type '(UnsafeMutableRawPointer) -> Int32'; expected '(UnsafeMutableRawPointer, UnsafeRawPointer, UnsafeMutablePointer<UnsafeMutableRawPointer?>) -> Int32'
// INVALID-TYPES-COUNT-1: error: function 'AddRef' in the 'COM' module has type '(UnsafeRawPointer) -> UInt32'; expected '(UnsafeMutableRawPointer) -> UInt32'
// INVALID-TYPES-COUNT-1: error: function 'Release' in the 'COM' module has type '(UnsafeMutableRawPointer) -> Int32'; expected '(UnsafeMutableRawPointer) -> UInt32'
// AMBIGUOUS-COUNT-1: error: multiple direct C entry points named 'QueryInterface' found in the 'COM' module
// AGGREGATED-COUNT-1: error: function 'AggregatedQueryInterface' not found in the 'COM' module
// AGGREGATED-COUNT-1: error: function 'AggregatedAddRef' not found in the 'COM' module
// AGGREGATED-COUNT-1: error: function 'AggregatedRelease' not found in the 'COM' module
// MISSING-DEFAULTS: error: type 'Widget' does not conform to protocol 'ISwiftObject'
