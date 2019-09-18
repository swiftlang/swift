// RUN: %empty-directory(%t)

// Build the protocol library and conforming library to include PrivateProtocol.
// RUN: %target-swift-frontend -emit-module -DPROTOCOL_LIB -DBEFORE %s -module-name protocol_lib -emit-module-path %t/protocol_lib.swiftmodule
// RUN: %target-swift-frontend -emit-module -DCONFORMS_LIB %s -module-name conforms_lib -emit-module-path %t/conforms_lib.swiftmodule -I %t
// RUN: %target-swift-frontend -emit-module -DINHERITS_LIB %s -module-name inherits_lib -emit-module-path %t/inherits_lib.swiftmodule -I %t
// RUN: %target-swift-frontend -emit-module -DCYCLIC_DEPENDENCY_LIB %s -module-name cyclic_dependency_lib -emit-module-path %t/cyclic_dependency_lib.swiftmodule -I %t

// Rebuild the protocol library to omit PrivateProtocol.
// RUN: %target-swift-frontend -emit-module -DPROTOCOL_LIB -DAFTER %s -module-name protocol_lib -emit-module-path %t/protocol_lib.swiftmodule

// The conforming library's types should still deserialize.
// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print conforms_lib -I %t | %FileCheck %s

// Protocols that use a missing protocol have to disappear (see the FileCheck
// lines).
// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print inherits_lib -I %t > %t/inherits_lib.txt
// RUN: %FileCheck -check-prefix=CHECK-INHERITS %s < %t/inherits_lib.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-INHERITS %s < %t/inherits_lib.txt

// We can't yet handle protocols with dependencies within the same module that
// can't be deserialized
// RUN: not --crash %target-swift-ide-test -print-module -source-filename %s -module-to-print cyclic_dependency_lib -I %t

#if PROTOCOL_LIB

#if BEFORE
public protocol PrivateProtocol {}
#endif

#elseif CONFORMS_LIB
import protocol_lib

public protocol PublicProtocol {}

// CHECK: class C1 : PublicProtocol {
public class C1 : PrivateProtocol, PublicProtocol {}

// CHECK: class C2 {
public class C2 {}

// CHECK: extension C2 : PublicProtocol {
extension C2 : PrivateProtocol, PublicProtocol {}

// CHECK: enum E1 : PublicProtocol {
public enum E1 : PrivateProtocol, PublicProtocol {}

// CHECK: enum E2 {
public enum E2 {}

// CHECK: extension E2 : PublicProtocol {
extension E2 : PrivateProtocol, PublicProtocol {}

// CHECK: struct S1 : PublicProtocol {
public struct S1 : PrivateProtocol, PublicProtocol {}

// CHECK: struct S2 {
public struct S2 {}

// CHECK: extension S2 : PublicProtocol {
extension S2 : PrivateProtocol, PublicProtocol {}

#elseif INHERITS_LIB
import protocol_lib

// NEGATIVE-INHERITS-NOT: protocol InheritsPrivateProtocol
public protocol InheritsPrivateProtocol : PrivateProtocol {}

// NEGATIVE-INHERITS-NOT: protocol TransitivelyInheritsPrivateProtocol
public protocol TransitivelyInheritsPrivateProtocol : InheritsPrivateProtocol {}

// NEGATIVE-INHERITS-NOT: protocol RequiresPrivateProtocol
public protocol RequiresPrivateProtocol {
  associatedtype Assoc: PrivateProtocol
}

// CHECK-INHERITS: protocol HasAssoc
public protocol HasAssoc {
  associatedtype Assoc
}

// NEGATIVE-INHERITS-NOT: protocol RequiresPrivateProtocolRefinement
public protocol RequiresPrivateProtocolRefinement : HasAssoc where Assoc: PrivateProtocol {}

#elseif CYCLIC_DEPENDENCY_LIB
import protocol_lib

public protocol CycleA : PrivateProtocol {
  associatedtype Assoc: CycleB
}

public protocol CycleB {
  associatedtype Assoc: CycleA
}

#endif
