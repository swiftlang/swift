// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -module-name M -o %t/M.swiftmodule \
// RUN:   -emit-clang-header-path %t/M-Swift.h \
// RUN:   -experimental-allow-module-with-compiler-errors
// RUN: %FileCheck --input-file %t/M-Swift.h %s

// RUN: %target-swift-frontend -emit-module %s -module-name M -o %t/M.swiftmodule \
// RUN:   -emit-clang-header-path %t/M-Swift-cxx.h -cxx-interoperability-mode=default \
// RUN:   -experimental-allow-module-with-compiler-errors
// RUN: %FileCheck --input-file %t/M-Swift-cxx.h %s

// Used to crash in hasExposeNotCxxAttr: @_nonSendable synthesizes an extension
// bound directly to Inner, whose parent extension has no extended nominal.

extension DoesNotExist {
  @_nonSendable public class Inner {
    public func foo() {}
  }
  public struct S { public var x: Int }
  public enum E { case a, b }
  public func f() {}
}

// CHECK: #ifndef M_SWIFT_H
// CHECK-NOT: Inner
// CHECK-NOT: foo
// CHECK-NOT: {{(struct|class|enum) (S|E)[^A-Za-z0-9_]}}
