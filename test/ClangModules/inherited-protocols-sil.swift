// RUN: %target-swift-frontend -sdk "" -emit-sil %s -import-objc-header %S/Inputs/inherited-protocols-sil.h -O

// REQUIRES: objc_interop

// <rdar://problem/24547884> Protocol Extensions May Crash Swift Compiler when Whole-Module Optimization is Enabled

extension SubProto {
  func foo() -> Impl {
    return Impl(child: self)
  }
}

_ = Impl().foo()
