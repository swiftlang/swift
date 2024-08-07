// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -typecheck -module-name UseCoreFoundation -enable-experimental-cxx-interop -clang-header-expose-decls=all-public -emit-clang-header-path %t/UseCoreFoundation.h
// RUN: %FileCheck %s < %t/UseCoreFoundation.h

// REQUIRES: objc_interop

import CoreFoundation
import Foundation

public func foobar(_ a: CFData) -> Bool {
    true
}

public func networkThing() -> in_addr? {
    return nil
}

// CHECK: SWIFT_EXTERN bool $s17UseCoreFoundation6foobarySbSo9CFDataRefaF(CFDataRef _Nonnull a) SWIFT_NOEXCEPT SWIFT_CALL; // foobar(_:)
// CHECK: SWIFT_INLINE_THUNK swift::Optional<in_addr> networkThing() noexcept SWIFT_SYMBOL("s:17UseCoreFoundation12networkThingSo7in_addrVSgyF") SWIFT_WARN_UNUSED_RESULT {
