// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Library.swiftmodule -parse-as-library %t/Library.swift -enable-library-evolution
// RUN: %target-swift-frontend -primary-file %t/Client.swift -I %t -emit-ir | %FileCheck %s

// REQUIRES: OS=macosx

//--- Library.swift

public protocol P { }
public protocol Q: P { }

public protocol StrongProtoWithAssoc {
  associatedtype Assoc: P
}

public protocol StrongProtoWithWeakLinkedAssoc {
  @_weakLinked associatedtype Assoc: P
}

public struct LibConformsToP: P { }

public protocol StrongProtoWithNewAssoc {
  @available(macOS 50, *)
  associatedtype NewerAssoc: P = LibConformsToP
}

@available(macOS 50, *)
public protocol WeakProtoWithAssoc {
  associatedtype Assoc: P
}

@available(macOS 50, *)
public protocol WeakProtoWithStrongInheritedAssoc: StrongProtoWithAssoc where Self.Assoc: Q { }


//--- Client.swift

import Library

struct ConformsToP: P { }
struct ConformsToQ: Q { }

// CHECK: @"$s7Library20StrongProtoWithAssocP0E0AC_AA1PTn" = external global %swift.protocol_requirement, align 4
struct ConformsToStrongProtoWithAssoc: StrongProtoWithAssoc {
  typealias Assoc = ConformsToP
}


// CHECK: @"$s7Library30StrongProtoWithWeakLinkedAssocP0G0AC_AA1PTn" = extern_weak global %swift.protocol_requirement, align 4
struct ConformsToStrongProtoWithWeakLinkedAssoc: StrongProtoWithWeakLinkedAssoc {
  typealias Assoc = ConformsToP
}

// CHECK: @"$s7Library23StrongProtoWithNewAssocP05NewerF0AC_AA1PTn" = extern_weak global %swift.protocol_requirement, align 4
// CHECK: @"$s10NewerAssoc7Library018StrongProtoWithNewB0PTl" = extern_weak global %swift.protocol_requirement, align 4
struct ConformsToStrongProtoWithNewAssoc: StrongProtoWithNewAssoc {
  typealias NewAssoc = ConformsToP
}

// CHECK: @"$s7Library18WeakProtoWithAssocP0E0AC_AA1PTn" = extern_weak global %swift.protocol_requirement, align 4
@available(macOS 50, *)
struct ConformsToWeakProtoWithAssoc: WeakProtoWithAssoc {
  typealias Assoc = ConformsToP
}

// CHECK: @"$s7Library33WeakProtoWithStrongInheritedAssocP0G0AA0ecdG0P_AA1QTn" = extern_weak global %swift.protocol_requirement, align 4
@available(macOS 50, *)
struct ConformsToWeakProtoWithStrongInheritedAssoc: WeakProtoWithStrongInheritedAssoc {
  typealias Assoc = ConformsToQ
}
