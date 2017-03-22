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

// CHECK-DAG: @_T09dllexport2ciAA1cCv = dllexport global %T9dllexport1cC* null, align 4
// CHECK-DAG: @_T09dllexport1pMp = dllexport constant %swift.protocol
// CHECK-DAG: @_T09dllexport1cCMn = dllexport constant
// CHECK-DAG: @_T09dllexport1cCML = dllexport global %swift.type* null, align 4
// CHECK-DAG: @_T09dllexport1dCML = dllexport global %swift.type* null, align 4
// CHECK-DAG: @_T09dllexport1cCN = dllexport alias %swift.type
// CHECK-DAG: @_T09dllexport1dCN = dllexport alias %swift.type, bitcast ({{.*}})
// CHECK-DAG-OPT: @_T09dllexport1dC1m33_C57BA610BA35E21738CC992438E660E9LLyyF = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG-OPT: @_T09dllexport1dCACycfc = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG-OPT: @_T09dllexport1cCACycfc = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG-OPT: @_T09dllexport1cCACycfC = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG: define dllexport swiftcc %swift.refcounted* @_T09dllexport1cCfd(%T9dllexport1cC*{{.*}})
// CHECK-DAG-NO-OPT: define dllexport swiftcc %T9dllexport1cC* @_T09dllexport1cCACycfc(%T9dllexport1cC*)
// CHECK-DAG-NO-OPT: define dllexport swiftcc %T9dllexport1cC* @_T09dllexport1cCACycfC(%swift.type*)
// CHECK-DAG: define dllexport swiftcc i8* @_T09dllexport2ciAA1cCfau()
// CHECK-DAG-NO-OPT: define dllexport swiftcc void @_T09dllexport1dC1m33_C57BA610BA35E21738CC992438E660E9LLyyF(%T9dllexport1dC*)
// CHECK-DAG-NO-OPT: define dllexport swiftcc void @_T09dllexport1dCfD(%T9dllexport1dC*)
// CHECK-DAG: define dllexport swiftcc %swift.refcounted* @_T09dllexport1dCfd(%T9dllexport1dC*{{.*}})
// CHECK-DAG: define dllexport %swift.type* @_T09dllexport1cCMa()
// CHECK-DAG: define dllexport %swift.type* @_T09dllexport1dCMa()
// CHECK-DAG-NO-OPT: define dllexport swiftcc %T9dllexport1dC* @_T09dllexport1dCACycfc(%T9dllexport1dC*)
// CHECK-DAG-OPT: define dllexport swiftcc void @_T09dllexport1dCfD(%T9dllexport1dC*)

