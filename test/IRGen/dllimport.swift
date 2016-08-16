// RUN: %swift -target thumbv7--windows-itanium -emit-ir -parse-as-library -parse-stdlib -module-name dllimport %s -o - -enable-source-import -I %S | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-NO-OPT
// RUN: %swift -target thumbv7--windows-itanium -O -emit-ir -parse-as-library -parse-stdlib -module-name dllimport %s -o - -enable-source-import -I %S | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-OPT

// REQUIRES: CODEGENERATOR=ARM

import dllexport

public func get_ci() -> dllexport.c {
  return dllexport.ci
}

public func get_c_type() -> dllexport.c.Type {
  return dllexport.c
}

public class d : c {
  override init() {
    super.init()
  }

  @inline(never)
  func f(_ : dllexport.c) { }
}

struct s : p {
  func f() { }
}

func f(di : d) {
  di.f(get_ci())
}

func blackhole<F>(_ : F) { }

public func g() {
  blackhole({ () -> () in })
}

// CHECK-DAG: @_swift_retain = external dllimport global void (%swift.refcounted*)
// CHECK-DAG-NO-OPT: @_swift_release = external dllimport global void (%swift.refcounted*)
// CHECK-DAG-NO-OPT: @_swift_deallocObject = external dllimport global void (%swift.refcounted*, i32, i32)*
// CHECK-DAG-NO-OPT: @_swift_allocObject = external dllimport global %swift.refcounted* (%swift.type*, i32, i32)*
// CHECK-DAG-NO-OPT: @_TMT_ = external dllimport global %swift.full_type
// CHECK-DAG: @_TWVBo = external dllimport global i8*
// CHECK-DAG: @_TMC9dllexport1c = external dllimport global %swift.type
// CHECK-DAG: @_TMp9dllexport1p = external dllimport global %swift.protocol
// CHECK-DAG: @_swift_slowAlloc = external dllimport global i8* (i32, i32)*
// CHECK-DAG: @_swift_slowDealloc = external dllimport global void (i8*, i32, i32)*
// CHECK-DAG: declare dllimport i8* @_TF9dllexportau2ciCS_1c()
// CHECK-DAG: declare dllimport %swift.type* @_TMaC9dllexport1c()
// CHECK-DAG: declare dllimport void @swift_deallocClassInstance(%swift.refcounted*, i32, i32)
// CHECK-DAG: declare dllimport %swift.refcounted* @_TFC9dllexport1cd(%C9dllexport1c*)
// CHECK-DAG-NO-OPT: define linkonce_odr hidden void @rt_swift_retain(%swift.refcounted*)
// CHECK-DAG-NO-OPT: define linkonce_odr hidden i8* @rt_swift_slowAlloc(i32, i32)
// CHECK-DAG-NO-OPT: define linkonce_odr hidden void @rt_swift_slowDealloc(i8*, i32, i32)
// CHECK-DAG-NO-OPT: define linkonce_odr hidden void @rt_swift_release(%swift.refcounted*)
// CHECK-OPT-DAG: declare dllimport void @swift_deletedMethodError()

