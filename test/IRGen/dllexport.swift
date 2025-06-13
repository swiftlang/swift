// RUN: %swift -target %host_triple -emit-ir -parse-as-library -disable-legacy-type-info -module-name dllexport %s -o - | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-NO-OPT
// RUN: %swift -target %host_triple -O -emit-ir -parse-as-library -disable-legacy-type-info -module-name dllexport %s -o - | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-OPT

// REQUIRES: OS=windows-msvc

import Swift

public protocol p {
  func f()
}

open class c {
  public init() { }
}

public var ci : c = c()

open class d {
  private func m() -> Never {
    fatalError()
  }
}

// CHECK-OPT-NOT: @"$s9dllexport1dCACycfc"

// CHECK-DAG: @"$s9dllexport2ciAA1cCvp" = dllexport global ptr null
// CHECK-DAG: @"$s9dllexport1pMp" = dllexport constant
// CHECK-DAG: @"$s9dllexport1cCMn" = dllexport constant
// CHECK-DAG: @"$s9dllexport1cCN" = dllexport alias %swift.type
// CHECK-DAG: @"$s9dllexport1dCN" = dllexport alias %swift.type
// CHECK-OPT-DAG: @"$s9dllexport1dC1m33_C57BA610BA35E21738CC992438E660E9LLs5NeverOyF" = dllexport alias void (), ptr @_swift_dead_method_stub
// CHECK-DAG: define dllexport swiftcc ptr @"$s9dllexport1cCfd"(ptr{{.*}})
// CHECK-DAG: define dllexport swiftcc ptr @"$s9dllexport1cCACycfc"(ptr{{.*}} swiftself %0)
// CHECK-DAG: define dllexport swiftcc {{(noalias )?}}ptr @"$s9dllexport1cCACycfC"(ptr swiftself %0)
// CHECK-DAG: define dllexport swiftcc {{(noundef )?(nonnull )?}}ptr @"$s9dllexport2ciAA1cCvau"()
// CHECK-NO-OPT-DAG: define dllexport swiftcc void @"$s9dllexport1dC1m33_C57BA610BA35E21738CC992438E660E9LLs5NeverOyF"(ptr swiftself %0)
// CHECK-DAG: define dllexport swiftcc void @"$s9dllexport1dCfD"(ptr swiftself %0)
// CHECK-DAG: define dllexport swiftcc ptr @"$s9dllexport1dCfd"(ptr{{.*}})
// CHECK-DAG: define dllexport swiftcc %swift.metadata_response @"$s9dllexport1cCMa"(i64 %0)
// CHECK-DAG: define dllexport swiftcc %swift.metadata_response @"$s9dllexport1dCMa"(i64 %0)
// CHECK-NO-OPT-DAG: define hidden swiftcc ptr @"$s9dllexport1dCACycfc"(ptr swiftself %0)
