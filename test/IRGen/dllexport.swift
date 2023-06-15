// RUN: %swift %use_no_opaque_pointers -target thumbv7--windows-itanium -emit-ir -parse-as-library -disable-legacy-type-info -parse-stdlib -module-name dllexport %s -o - | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-NO-OPT
// RUN: %swift %use_no_opaque_pointers -target thumbv7--windows-itanium -O -emit-ir -parse-as-library -disable-legacy-type-info -parse-stdlib -module-name dllexport %s -o - | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-OPT
// RUN: %swift -target thumbv7--windows-itanium -emit-ir -parse-as-library -disable-legacy-type-info -parse-stdlib -module-name dllexport %s -o -
// RUN: %swift -target thumbv7--windows-itanium -O -emit-ir -parse-as-library -disable-legacy-type-info -parse-stdlib -module-name dllexport %s -o -


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

// CHECK-DAG: @"$s9dllexport2ciAA1cCvp" = dllexport global %T9dllexport1cC* null, align 4
// CHECK-DAG: @"$s9dllexport1pMp" = dllexport constant
// CHECK-DAG: @"$s9dllexport1cCMn" = dllexport constant
// CHECK-DAG: @"$s9dllexport1cCN" = dllexport alias %swift.type
// CHECK-DAG: @"$s9dllexport1dCN" = dllexport alias %swift.type, bitcast ({{.*}})
// CHECK-DAG-OPT: @"$s9dllexport1dC1m33_C57BA610BA35E21738CC992438E660E9LLyyF" = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG-OPT: @"$s9dllexport1dCACycfc" = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG-OPT: @"$s9dllexport1cCACycfc" = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG-OPT: @"$s9dllexport1cCACycfC" = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG: define dllexport swiftcc %swift.refcounted* @"$s9dllexport1cCfd"(%T9dllexport1cC*{{.*}})
// CHECK-DAG-NO-OPT: define dllexport swiftcc %T9dllexport1cC* @"$s9dllexport1cCACycfc"(%T9dllexport1cC* %0)
// CHECK-DAG-NO-OPT: define dllexport swiftcc %T9dllexport1cC* @"$s9dllexport1cCACycfC"(%swift.type* %0)
// CHECK-DAG: define dllexport swiftcc {{(nonnull )?}}i8* @"$s9dllexport2ciAA1cCvau"()
// CHECK-DAG-NO-OPT: define dllexport swiftcc void @"$s9dllexport1dC1m33_C57BA610BA35E21738CC992438E660E9LLyyF"(%T9dllexport1dC* %0)
// CHECK-DAG-NO-OPT: define dllexport swiftcc void @"$s9dllexport1dCfD"(%T9dllexport1dC* %0)
// CHECK-DAG: define dllexport swiftcc %swift.refcounted* @"$s9dllexport1dCfd"(%T9dllexport1dC*{{.*}})
// CHECK-DAG: define dllexport swiftcc %swift.metadata_response @"$s9dllexport1cCMa"(i32 %0)
// CHECK-DAG: define dllexport swiftcc %swift.metadata_response @"$s9dllexport1dCMa"(i32 %0)
// CHECK-DAG-NO-OPT: define dllexport swiftcc %T9dllexport1dC* @"$s9dllexport1dCACycfc"(%T9dllexport1dC* %0)
// CHECK-DAG-OPT: define dllexport swiftcc void @"$s9dllexport1dCfD"(%T9dllexport1dC* %0)

