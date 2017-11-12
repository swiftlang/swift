// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name cross_module_optional_protocol_reqt -c -emit-module-path %t/cross_module_optional_protocol_reqt~partial.swiftmodule -primary-file %s %S/Inputs/cross_module_optional_protocol_reqt_2.swift -import-objc-header %S/Inputs/cross_module_optional_protocol_reqt.h -o /dev/null
// RUN: %target-swift-frontend -module-name cross_module_optional_protocol_reqt -c -emit-module-path %t/cross_module_optional_protocol_reqt_2~partial.swiftmodule %s -primary-file %S/Inputs/cross_module_optional_protocol_reqt_2.swift -import-objc-header %S/Inputs/cross_module_optional_protocol_reqt.h -o /dev/null
// RUN: %target-swift-frontend -module-name cross_module_optional_protocol_reqt -emit-module -emit-module-path %t/cross_module_optional_protocol_reqt.swiftmodule %t/cross_module_optional_protocol_reqt~partial.swiftmodule %t/cross_module_optional_protocol_reqt_2~partial.swiftmodule -import-objc-header %S/Inputs/cross_module_optional_protocol_reqt.h
// RUN: %target-swift-frontend -I %t -typecheck %s -DTEST -module-name main
// REQUIRES: objc_interop

#if TEST

import cross_module_optional_protocol_reqt

func test(_ foo: Foo) {
  // At one point this forced deserialization of the conformance to ObjCProto,
  // which was corrupted by the presence of the optional requirement with no
  // witness.
  foo.nonoptionalMethod()
  foo.nonoptionalMethod2()
}

#else // TEST

public protocol SwiftProto: ObjCProto {}

public class Foo: ObjCFoo, SwiftProto {
  public func nonoptionalMethod() {}
  public func nonoptionalMethod2() {}
}

#endif // TEST
