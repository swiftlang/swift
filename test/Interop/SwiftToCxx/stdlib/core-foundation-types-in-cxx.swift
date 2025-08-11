// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -module-name UseCoreFoundation -enable-experimental-cxx-interop -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/UseCoreFoundation.h
// RUN: %FileCheck %s < %t/UseCoreFoundation.h

// RUN: echo "#include <netinet/in.h>" > %t/full-header.h
// RUN: cat %t/UseCoreFoundation.h >> %t/full-header.h
// RUN: %target-interop-build-clangxx -std=gnu++20 -fobjc-arc -c -x objective-c++-header %t/full-header.h -o %t/o.o

// REQUIRES: objc_interop

import CoreFoundation
import Foundation

public func foobar(_ a: CFData) -> Bool {
    true
}

public func returnsCFDate() -> CFDate? {
    nil
}

public func takesCFDate(x: CFDate?) {}

public func networkThing() -> in_addr? {
    return nil
}

public enum MyEnum {
    case value(in_addr)
}

// CHECK: SWIFT_EXTERN bool $s17UseCoreFoundation6foobarySbSo9CFDataRefaF(CFDataRef _Nonnull a) SWIFT_NOEXCEPT SWIFT_CALL; // foobar(_:)
// CHECK: SWIFT_EXTERN CFDateRef _Nullable $s17UseCoreFoundation13returnsCFDateSo0E3RefaSgyF(void) SWIFT_NOEXCEPT SWIFT_CALL; // returnsCFDate()
// CHECK: SWIFT_EXTERN void $s17UseCoreFoundation11takesCFDate1xySo0E3RefaSg_tF(CFDateRef _Nullable x) SWIFT_NOEXCEPT SWIFT_CALL; // takesCFDate(x:)

// CHECK: SWIFT_INLINE_THUNK swift::Optional<in_addr> networkThing() noexcept SWIFT_SYMBOL("s:17UseCoreFoundation12networkThingSo7in_addrVSgyF") SWIFT_WARN_UNUSED_RESULT {
// CHECK: SWIFT_INLINE_THUNK CFDateRef _Nullable returnsCFDate() noexcept SWIFT_SYMBOL("s:17UseCoreFoundation13returnsCFDateSo0E3RefaSgyF") SWIFT_WARN_UNUSED_RESULT {
// CHECK: SWIFT_INLINE_THUNK void takesCFDate(CFDateRef _Nullable x) noexcept SWIFT_SYMBOL("s:17UseCoreFoundation11takesCFDate1xySo0E3RefaSg_tF") {
