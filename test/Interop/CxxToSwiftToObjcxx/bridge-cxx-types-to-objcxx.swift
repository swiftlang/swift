// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -module-name UseCxxTy -typecheck -verify -emit-clang-header-path %t/UseCxxTy.h -I %t -I %S/Inputs -enable-experimental-cxx-interop -clang-header-expose-decls=all-public -target arm64-apple-macosx13.3
// RUN: %FileCheck %s < %t/UseCxxTy.h

// REQUIRES: OS=macosx

// This test is slow, because it triggers Clang module build for the C++ stdlib and Foundation.
// REQUIRES: long_test

import Foundation
import MyCxxModule

@objc class C: NSObject {
  @objc var x: X? = nil
}

// CHECK: @property (nonatomic) X * _Nullable x;
