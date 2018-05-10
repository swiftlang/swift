// RUN: %swift -target thumbv7--windows-itanium -emit-ir -parse-as-library -parse-stdlib -module-name dllexport %s -o - | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-NO-OPT
// RUN: %swift -target thumbv7--windows-itanium -O -emit-ir -parse-as-library -parse-stdlib -module-name dllexport %s -o - | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-OPT

// REQUIRES: CODEGENERATOR=ARM

enum Never {}

@_silgen_name("_swift_fatalError")
func fatalError() -> Never

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

// CHECK-DAG: @"$S9dllexport2ciAA1cCvp" = dllexport global %T9dllexport1cC* null, align 4
// CHECK-DAG: @"$S9dllexport1pMp" = dllexport constant %swift.protocol
// CHECK-DAG: @"$S9dllexport1cCMn" = dllexport constant
// CHECK-DAG: @"$S9dllexport1cCN" = dllexport alias %swift.type
// CHECK-DAG: @"$S9dllexport1dCN" = dllexport alias %swift.type, bitcast ({{.*}})
// CHECK-DAG-OPT: @"$S9dllexport1dC1m33_C57BA610BA35E21738CC992438E660E9LLyyF" = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG-OPT: @"$S9dllexport1dCACycfc" = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG-OPT: @"$S9dllexport1cCACycfc" = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG-OPT: @"$S9dllexport1cCACycfC" = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG: define dllexport swiftcc %swift.refcounted* @"$S9dllexport1cCfd"(%T9dllexport1cC*{{.*}})
// CHECK-DAG-NO-OPT: define dllexport swiftcc %T9dllexport1cC* @"$S9dllexport1cCACycfc"(%T9dllexport1cC*)
// CHECK-DAG-NO-OPT: define dllexport swiftcc %T9dllexport1cC* @"$S9dllexport1cCACycfC"(%swift.type*)
// CHECK-DAG: define dllexport swiftcc i8* @"$S9dllexport2ciAA1cCvau"()
// CHECK-DAG-NO-OPT: define dllexport swiftcc void @"$S9dllexport1dC1m33_C57BA610BA35E21738CC992438E660E9LLyyF"(%T9dllexport1dC*)
// CHECK-DAG-NO-OPT: define dllexport swiftcc void @"$S9dllexport1dCfD"(%T9dllexport1dC*)
// CHECK-DAG: define dllexport swiftcc %swift.refcounted* @"$S9dllexport1dCfd"(%T9dllexport1dC*{{.*}})
// CHECK-DAG: define dllexport swiftcc %swift.metadata_response @"$S9dllexport1cCMa"(i32)
// CHECK-DAG: define dllexport swiftcc %swift.metadata_response @"$S9dllexport1dCMa"(i32)
// CHECK-DAG-NO-OPT: define dllexport swiftcc %T9dllexport1dC* @"$S9dllexport1dCACycfc"(%T9dllexport1dC*)
// CHECK-DAG-OPT: define dllexport swiftcc void @"$S9dllexport1dCfD"(%T9dllexport1dC*)

