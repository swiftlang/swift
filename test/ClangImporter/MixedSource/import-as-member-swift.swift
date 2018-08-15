// RUN: %target-swift-frontend -import-objc-header %S/Inputs/import-as-member-swift.h -typecheck -enable-objc-interop -disable-objc-attr-requires-foundation-module %s

@objc internal class Outer {}

_ = Outer.Nested()
