// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name M -emit-module -o %t/M.swiftmodule \
// RUN:   -enable-library-evolution -cxx-interoperability-mode=default \
// RUN:   -emit-clang-header-path %t/M.h -emit-clang-header-min-access internal \
// RUN:   -experimental-lazy-typecheck -experimental-skip-non-exportable-decls \
// RUN:   -experimental-skip-non-inlinable-function-bodies
// RUN: %FileCheck %s < %t/M.h

// With lazy type checking, the extension of an unresolvable type is never
// diagnosed or marked invalid. Make sure the header printer skips it instead
// of crashing.

extension DoesNotExist {
  func f() {}
}

public final class A {}
public final class Z {}

// CHECK-NOT: DoesNotExist
// CHECK: class SWIFT_SYMBOL("s:1M1AC") A final
// CHECK: class SWIFT_SYMBOL("s:1M1ZC") Z final
