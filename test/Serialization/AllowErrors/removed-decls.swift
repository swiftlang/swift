// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/swiftmods %t/objcmods %t/objc
// RUN: %{python} %utils/split_file.py -o %t %s



// Create a module A, then B that depends on A, replace A with an empty module,
// and then try make a C that depends on B

// RUN: %target-swift-frontend -module-name A -emit-module -o %t/swiftmods/A.swiftmodule %t/a.swift
// RUN: %target-swift-frontend -module-name B -emit-module -o %t/swiftmods/B.swiftmodule -I %t/swiftmods %t/b.swift
// RUN: %target-swift-frontend -module-name A -emit-module -o %t/swiftmods/A.swiftmodule %t/bada.swift
// RUN: %target-swift-frontend -wmo -module-name C -emit-module -o %t/swiftmods/C.swiftmodule -I %t/swiftmods -experimental-allow-module-with-compiler-errors -index-store-path %t/idx %t/c.swift
// RUN: not --crash %target-swift-frontend -module-name C -emit-module -o %t/swiftmods/C.swiftmodule -I %t/swiftmods %t/c.swift

// Now do a similar thing but just use different headers instead (ie. to test
// loading from a PCM rather than swiftmodule)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name B -emit-module -o %t/objcmods/B.swiftmodule -I %t/objc -module-cache-path %t/mcp %t/b.swift
// RUN: mv %t/objc/bada.h %t/objc/a.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name C -emit-module -o %t/objcmods/C.swiftmodule -I %t/objcmods -I %t/objc -module-cache-path %t/mcp -experimental-allow-module-with-compiler-errors -index-store-path %t/idx %t/c.swift
// RUN: not --crash %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name C -emit-module -o %t/objcmods/C.swiftmodule -I %t/objcmods -I %t/objc -module-cache-path %t/mcp %t/c.swift

// BEGIN a.swift
public protocol ProtoA {}
public protocol MissingProto {}
open class MissingClass: ProtoA {}

// BEGIN bada.swift
public protocol ProtoA {}

// BEGIN objc/a.h
@import Foundation;
@protocol ProtoA
@end
@protocol MissingProto
@end
@interface MissingClass <ProtoA>
@end

// BEGIN objc/bada.h
@import Foundation;
@protocol ProtoA
@end

// BEGIN objc/module.modulemap
module A {
  header "a.h"
}

// BEGIN b.swift
import A

public protocol ProtoB: MissingProto {}
open class ClassB: MissingProto {}
open class InheritingClassB: MissingClass {}

public protocol ATProto {
  associatedtype Value
}
public protocol ReqProto: ATProto {
  static func thing(_ value: Value)
}
extension ReqProto where Value: ProtoA {
  public static func thing(_ value: Value) {}
}
public enum EnumB: ReqProto  {
  public typealias Value = MissingClass
  case a
}

// BEGIN c.swift
import B

func useB(p: ProtoB, c: ClassB, i: InheritingClassB, e: EnumB) {
  print("p:\(p) c:\(c) i:\(i) e:\(e)")
  EnumB.thing(i)
}

public protocol ProtoC: ProtoB {}
public class ClassC: ClassB {}
public class ClassC2: InheritingClassB {}

public struct AllAsMembers {
  let p: ProtoB
  let c: ClassB
  let i: InheritingClassB
  let e: EnumB
}

extension ProtoB {
  func newProtoBMethod() {}
}
extension ClassB {
  func newClassBMethod() {}
}
extension InheritingClassB {
  func newInheritingClassBMethod() {}
}
extension EnumB {
  func newEnumBMethod() {}
}
