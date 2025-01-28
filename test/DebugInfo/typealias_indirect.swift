// RUN: %target-swift-frontend %s -emit-ir -parse-as-library -module-name a -g -o - | %FileCheck %s
// CHECK: !DIDerivedType(tag: DW_TAG_typedef, name: "$s1a10LocalAliasaD", {{.*}}baseType: ![[BASETY:[0-9]+]]
// CHECK: ![[BASETY]]{{.*}}$sSbD
public class MyClass<A, B> {}
public typealias LocalAlias = Bool
public typealias ClassAlias = MyClass<LocalAlias, Bool>
public func use(cls: ClassAlias?) {}

