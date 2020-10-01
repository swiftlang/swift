// RUN: %target-swift-frontend -import-objc-header %S/Inputs/import-as-member-swift.h -typecheck -enable-objc-interop -disable-objc-attr-requires-foundation-module %s

@objc internal class Outer {}

@objc(OuterByObjCName_ObjC)
internal class OuterByObjCName_Swift {}

@objc(OuterBySwiftName_ObjC)
internal class OuterBySwiftName_Swift {}

_ = Outer.Nested(a: 1)
_ = OuterByObjCName_Swift.Nested(b: 2)
_ = OuterBySwiftName_Swift.Nested(c: 3)
