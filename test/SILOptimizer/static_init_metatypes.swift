// RUN: %target-swift-frontend %s -parse-as-library -module-name=test -emit-sil | %FileCheck --check-prefix=SIL %s
// RUN: %target-swift-frontend %s -parse-as-library -O -module-name=test -emit-sil | %FileCheck --check-prefix=SIL %s

// RUN: %target-swift-frontend %s -parse-as-library -module-name=test -emit-ir | %FileCheck --check-prefix=IR %s
// RUN: %target-swift-frontend %s -parse-as-library -O -module-name=test -emit-ir | %FileCheck --check-prefix=IR %s

// RUN: %target-run-simple-swift(-parse-as-library)
// RUN: %target-run-simple-swift(-parse-as-library -O)

// REQUIRES: executable_test

// TODO: Resolve link-time error (unresolved external symbol $sSiN) to Windows
// XFAIL: OS=windows-msvc

public protocol MyProtocol {}
public protocol MyProtocol2 {}

public struct S: Hashable, Sendable, MyProtocol, MyProtocol2 {}
extension Int: MyProtocol, MyProtocol2 {}

public let metatype1 = Int.self
// SIL:      sil_global [let] @$s4test9metatype1Simvp : $@thin Int.Type = {
// SIL-NEXT:   %initval = metatype $@thin Int.Type
// SIL-NEXT: }
public let metatype2 = S.self
// SIL:      sil_global [let] @$s4test9metatype2AA1SVmvp : $@thin S.Type = {
// SIL-NEXT:   %initval = metatype $@thin S.Type
// SIL-NEXT: }
public let metatype3: Any.Type = Int.self
// SIL:      sil_global [let] @$s4test9metatype3ypXpvp : $@thick any Any.Type = {
// SIL-NEXT:   %0 = metatype $@thick Int.Type                  // user: %1
// SIL-NEXT:   %initval = init_existential_metatype %0, $@thick any Any.Type
// SIL-NEXT: }
public let metatype4: Any.Type = S.self
// SIL:      sil_global [let] @$s4test9metatype4ypXpvp : $@thick any Any.Type = {
// SIL-NEXT:   %0 = metatype $@thick S.Type                    // user: %1
// SIL-NEXT:   %initval = init_existential_metatype %0, $@thick any Any.Type
// SIL-NEXT: }
public let metatype5: any MyProtocol.Type = Int.self
// SIL:      sil_global [let] @$s4test9metatype5AA10MyProtocol_pXpvp : $@thick any MyProtocol.Type = {
// SIL-NEXT:   %0 = metatype $@thick Int.Type                  // user: %1
// SIL-NEXT:   %initval = init_existential_metatype %0, $@thick any MyProtocol.Type
// SIL-NEXT: }
public let metatype6: any MyProtocol.Type = S.self
// SIL:      sil_global [let] @$s4test9metatype6AA10MyProtocol_pXpvp : $@thick any MyProtocol.Type = {
// SIL-NEXT:   %0 = metatype $@thick S.Type                    // user: %1
// SIL-NEXT:   %initval = init_existential_metatype %0, $@thick any MyProtocol.Type
// SIL-NEXT: }
public let metatype7: any (MyProtocol & MyProtocol2).Type = Int.self
// SIL:      sil_global [let] @$s4test9metatype7AA10MyProtocol_AA0C9Protocol2pXpvp : $@thick any (MyProtocol & MyProtocol2).Type = {
// SIL-NEXT:   %0 = metatype $@thick Int.Type                  // user: %1
// SIL-NEXT:   %initval = init_existential_metatype %0, $@thick any (MyProtocol & MyProtocol2).Type
// SIL-NEXT: }
public let metatype8: any (MyProtocol & MyProtocol2).Type = S.self
// SIL:      sil_global [let] @$s4test9metatype8AA10MyProtocol_AA0C9Protocol2pXpvp : $@thick any (MyProtocol & MyProtocol2).Type = {
// SIL-NEXT:   %0 = metatype $@thick S.Type                    // user: %1
// SIL-NEXT:   %initval = init_existential_metatype %0, $@thick any (MyProtocol & MyProtocol2).Type
// SIL-NEXT: }
public let metatype9: any Hashable.Type = Int.self
// SIL: sil_global [let] @$s4test9metatype9SH_pXpvp : $@thick any Hashable.Type
// SIL-EMPTY:
public let metatype10: any Hashable.Type = S.self
// SIL: sil_global [let] @$s4test10metatype10SH_pXpvp : $@thick any Hashable.Type
// SIL-EMPTY:
public let metatype11: any (Hashable & Sendable).Type = Int.self
// SIL: sil_global [let] @$s4test10metatype11SH_s8SendablepXpvp : $@thick any (Hashable & Sendable).Type
// SIL-EMPTY:
public let metatype12: any (Hashable & Sendable).Type = S.self
// SIL: sil_global [let] @$s4test10metatype12SH_s8SendablepXpvp : $@thick any (Hashable & Sendable).Type
// SIL-EMPTY:
public let metatype13: any (Hashable & MyProtocol).Type = Int.self
// SIL: sil_global [let] @$s4test10metatype13SH_AA10MyProtocolpXpvp : $@thick any (Hashable & MyProtocol).Type
// SIL-EMPTY:
public let metatype14: any (Hashable & MyProtocol).Type = S.self
// SIL: sil_global [let] @$s4test10metatype14SH_AA10MyProtocolpXpvp : $@thick any (Hashable & MyProtocol).Type
// SIL-EMPTY:
public let metatype15: Any.Type = Array<Int>.self
// SIL: sil_global [let] @$s4test10metatype15ypXpvp : $@thick any Any.Type
// SIL-EMPTY:
public let metatype16: any (Hashable & Sendable).Type = Array<Int>.self
// SIL: sil_global [let] @$s4test10metatype16SH_s8SendablepXpvp : $@thick any (Hashable & Sendable).Type
// SIL-EMPTY:
public let metatype17: Any.Type = (Bool.random() ? Array<Int>.self : Array<Bool>.self).self
// SIL: sil_global [let] @$s4test10metatype17ypXpvp : $@thick any Any.Type
// SIL-EMPTY:
public let metatype18: Any.Type = Mirror.self // resilient
// SIL: sil_global [let] @$s4test10metatype18ypXpvp : $@thick any Any.Type
// SIL-EMPTY:
public var metatype19: Any.Type = (any MyProtocol).self
// SIL: sil_global @$s4test10metatype19ypXpvp : $@thick any Any.Type
// SIL-EMPTY:
public var metatype20: [Any.Type] = [(any MyProtocol).self]
// SIL: sil_global @$s4test10metatype20SayypXpGvp : $Array<any Any.Type>
// SIL-EMPTY:
public var metatype21: [Any.Type] = [Int.self]
// SIL: sil_global @$s4test10metatype21SayypXpGvp : $Array<any Any.Type>
// SIL-EMPTY:
public var metatype22: [Any.Type] = [S.self]
// SIL: sil_global @$s4test10metatype22SayypXpGvp : $Array<any Any.Type>
// SIL-EMPTY:

// Only to detect link-time missing symbols
@main
struct Main { static func main() { } }

// IR: @"$s4test9metatype3ypXpvp" = {{.*}}constant ptr @"$sSiN"
// IR: @"$s4test9metatype4ypXpvp" = {{.*}}constant ptr getelementptr inbounds{{.*}} ({{.*}}, ptr @"$s4test1SVMf", {{.*}})
// IR: @"$s4test9metatype5AA10MyProtocol_pXpvp" = {{.*}}constant <{ ptr, ptr }> <{ ptr @"$sSiN", ptr @"$sSi4test10MyProtocolAAWP" }>
// IR: @"$s4test9metatype6AA10MyProtocol_pXpvp" = {{.*}}constant <{ ptr, ptr }> <{ ptr getelementptr inbounds{{.*}} ({{.*}}, ptr @"$s4test1SVMf", {{.*}}), ptr @"$s4test1SVAA10MyProtocolAAWP" }>
// IR: @"$s4test9metatype7AA10MyProtocol_AA0C9Protocol2pXpvp" = {{.*}}constant <{ ptr, ptr, ptr }> <{ ptr @"$sSiN", ptr @"$sSi4test10MyProtocolAAWP", ptr @"$sSi4test11MyProtocol2AAWP" }>
// IR: @"$s4test9metatype8AA10MyProtocol_AA0C9Protocol2pXpvp" = {{.*}}constant <{ ptr, ptr, ptr }> <{ ptr getelementptr inbounds{{.*}} ({{.*}}, ptr @"$s4test1SVMf", {{.*}}), ptr @"$s4test1SVAA10MyProtocolAAWP", ptr @"$s4test1SVAA11MyProtocol2AAWP" }>
