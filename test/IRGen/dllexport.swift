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

// CHECK-DAG: @_Tv9dllexport2ciCS_1c = dllexport global %C9dllexport1c* null, align 4
// CHECK-DAG: @_TMp9dllexport1p = dllexport constant %swift.protocol
// CHECK-DAG: @_TMnC9dllexport1c = dllexport constant
// CHECK-DAG: @_TMLC9dllexport1c = dllexport global %swift.type* null, align 4
// CHECK-DAG: @_TMLC9dllexport1d = dllexport global %swift.type* null, align 4
// CHECK-DAG: @_TMC9dllexport1c = dllexport alias %swift.type
// CHECK-DAG: @_TMC9dllexport1d = dllexport alias %swift.type, bitcast ({{.*}})
// CHECK-DAG-OPT: @_TFC9dllexport1dP33_C57BA610BA35E21738CC992438E660E91mfT_T_ = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG-OPT: @_TFC9dllexport1dcfT_S0_ = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG-OPT: @_TFC9dllexport1ccfT_S0_ = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG-OPT: @_TFC9dllexport1cCfT_S0_ = dllexport alias void (), void ()* @_swift_dead_method_stub
// CHECK-DAG: define dllexport %swift.refcounted* @_TFC9dllexport1cd(%C9dllexport1c*{{.*}})
// CHECK-DAG-NO-OPT: define dllexport %C9dllexport1c* @_TFC9dllexport1ccfT_S0_(%C9dllexport1c*)
// CHECK-DAG-NO-OPT: define dllexport %C9dllexport1c* @_TFC9dllexport1cCfT_S0_(%swift.type*)
// CHECK-DAG: define dllexport i8* @_TF9dllexportau2ciCS_1c()
// CHECK-DAG-NO-OPT: define dllexport void @_TFC9dllexport1dP33_C57BA610BA35E21738CC992438E660E91mfT_T_(%C9dllexport1d*)
// CHECK-DAG-NO-OPT: define dllexport void @_TFC9dllexport1dD(%C9dllexport1d*)
// CHECK-DAG: define dllexport %swift.refcounted* @_TFC9dllexport1dd(%C9dllexport1d*{{.*}})
// CHECK-DAG: define dllexport %swift.type* @_TMaC9dllexport1c()
// CHECK-DAG: define dllexport %swift.type* @_TMaC9dllexport1d()
// CHECK-DAG-NO-OPT: define dllexport %C9dllexport1d* @_TFC9dllexport1dcfT_S0_(%C9dllexport1d*)
// CHECK-DAG-OPT: define dllexport void @_TFC9dllexport1dD(%C9dllexport1d*)

