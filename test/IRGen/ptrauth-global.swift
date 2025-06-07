// RUN: %swift-frontend -swift-version 4 -target arm64e-apple-ios12.0 -primary-file %s -emit-ir -module-name A | %FileCheck %s --check-prefix=CHECK
// RUN: %swift-frontend -swift-version 4 -target arm64e-apple-ios12.0 %s -primary-file %S/Inputs/ptrauth-global-2.swift -emit-ir -module-name A | %FileCheck %s --check-prefix=CHECK2

// REQUIRES: CPU=arm64e
// REQUIRES: OS=ios

// Make sure that the key at the definition of the global matches call sites.

// CHECK-DAG: @"$s1A9ContainerV3AllAA1GVySiGycAA1VVySiGcycvpZ" = constant %swift.function { {{.*}} @"$s1A9ContainerV3AllAA1GVySiGycAA1VVySiGcycvpZfiAGycAJcycfU_.ptrauth"
// CHECK-DAG: @"$s1A9ContainerV3AllAA1GVySiGycAA1VVySiGcycvpZfiAGycAJcycfU_.ptrauth" = {{.*}}@"$s1A9ContainerV3AllAA1GVySiGycAA1VVySiGcycvpZfiAGycAJcycfU_"{{.*}} i64 58141 }, section "llvm.ptrauth"



// CHECK2: define {{.*}}swiftcc void @"$s1A4testyyF"()
// CHECK2:  [[T:%.*]] = call swiftcc ptr @"$s1A9ContainerV3AllAA1GVySiGycAA1VVySiGcycvau"()
// CHECK2:  [[T1:%.*]] = getelementptr inbounds %swift.function, ptr [[T]], i32 0, i32 0
// CHECK2:  [[T4:%.*]] = load ptr, ptr [[T1]]
// CHECK2:  call swiftcc { ptr, ptr } [[T4]]({{.*}}) [ "ptrauth"(i32 0, i64 58141) ]

public struct G<T> {
    init(_ t: T) {}
}

public struct V<T> {
    var str: String = ""
    var str1: String = ""
    var str2: String = ""
    var str3: String = ""
    var str4: String = ""
    var str5: String = ""
    var str6: String = ""
    var str7: String = ""
    var str8: String = ""
    init(_ t: T) {}
}

// Because of the large parameter type the signature gets transformed by the
// large loadable pass. The types in the global initializer need to follow.
public struct Container {
  public static let All = { return { (_ v: V<Int>) in { return G(5) } } }
}
