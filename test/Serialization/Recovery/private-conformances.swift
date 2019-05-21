// RUN: %empty-directory(%t)

// Build the protocol library and conforming library to include PrivateProtocol.
// RUN: %target-swift-frontend -emit-module -DPROTOCOL_LIB -DBEFORE %s -module-name protocol_lib -emit-module-path %t/protocol_lib.swiftmodule
// RUN: %target-swift-frontend -emit-module -DCONFORMS_LIB %s -module-name conforms_lib -emit-module-path %t/conforms_lib.swiftmodule -I %t
// RUN: %target-swift-frontend -emit-module -DINHERITS_LIB %s -module-name inherits_lib -emit-module-path %t/inherits_lib.swiftmodule -I %t

// Rebuild the protocol library to omit PrivateProtocol.
// RUN: %target-swift-frontend -emit-module -DPROTOCOL_LIB -DAFTER %s -module-name protocol_lib -emit-module-path %t/protocol_lib.swiftmodule

// The conforming library's types should still deserialize.
// RUN: %target-swift-ide-test -print-module -source-filename %s -D CONFORMS_LIB -module-to-print conforms_lib -I %t | %FileCheck %s

// Make sure we *cannot* recover from a missing inherited protocol.
// RUN: not --crash %target-swift-ide-test -print-module -source-filename %s -D INHERITS_LIB -module-to-print inherits_lib -I %t

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

public protocol InheritsPrivateProtocol : PrivateProtocol {}

#endif