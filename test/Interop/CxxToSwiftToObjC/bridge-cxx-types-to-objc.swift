// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -module-name UseCxxModule -emit-objc-header -emit-objc-header-path %t/UseCxxTy.h -I %t -I %S/Inputs -clang-header-expose-decls=all-public -cxx-interoperability-mode=default
// RUN: %FileCheck %s < %t/UseCxxTy.h

import CxxModule

public func bar(x: Foo, y: UnsafeMutablePointer<UnsafeMutableRawPointer?>) {}

// CHECK: // Unavailable in C++: Swift global function 'bar(x:y:)'.
// TODO: the message above is not correct, this is actually unavailable in Obj-C.
