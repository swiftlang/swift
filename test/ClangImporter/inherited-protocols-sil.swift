// RUN: %target-swift-frontend -sdk "" -emit-sil %s -enable-objc-interop -import-objc-header %S/Inputs/inherited-protocols-sil.h -O

// <rdar://problem/24547884> Protocol Extensions May Crash Swift Compiler when Whole-Module Optimization is Enabled

extension SubProto {
  func foo() -> Impl {
    return Impl(child: self)
  }
}

_ = Impl().foo()
