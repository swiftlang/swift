// RUN: %swift -target thumbv7--windows-itanium -emit-ir -parse-as-library -parse-stdlib -module-name dllimport %s -o - -enable-source-import -I %S | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-NO-OPT
// RUN: %swift -target thumbv7--windows-itanium -O -emit-ir -parse-as-library -parse-stdlib -module-name dllimport -primary-file %s -o - -enable-source-import -I %S | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-OPT

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

// CHECK-NO-OPT-DAG: @_swift_allocObject = external dllimport global %swift.refcounted* (%swift.type*, i32, i32)*
// CHECK-NO-OPT-DAG: @_swift_deallocObject = external dllimport global void (%swift.refcounted*, i32, i32)*
// CHECK-NO-OPT-DAG: @_swift_release = external dllimport global void (%swift.refcounted*)
// CHECK-NO-OPT-DAG: @_swift_retain = external dllimport global void (%swift.refcounted*)
// CHECK-NO-OPT-DAG: @_swift_slowAlloc = external dllimport global i8* (i32, i32)*
// CHECK-NO-OPT-DAG: @_swift_slowDealloc = external dllimport global void (i8*, i32, i32)*
// CHECK-NO-OPT-DAG: @_T09dllexport1cCN = external dllimport global %swift.type
// CHECK-NO-OPT-DAG: @_T09dllexport1pMp = external dllimport global %swift.protocol
// CHECK-NO-OPT-DAG: @_T0ytN = external dllimport global %swift.full_type
// CHECK-NO-OPT-DAG: @_T0BoWV = external dllimport global i8*
// CHECK-NO-OPT-DAG: declare dllimport swiftcc i8* @_T09dllexport2ciAA1cCfau()
// CHECK-NO-OPT-DAG: declare dllimport swiftcc %swift.refcounted* @_T09dllexport1cCfd(%T9dllexport1cC* swiftself)
// CHECK-NO-OPT-DAG: declare dllimport %swift.type* @_T09dllexport1cCMa()
// CHECK-NO-OPT-DAG: declare dllimport void @swift_deallocClassInstance(%swift.refcounted*, i32, i32)
// CHECK-NO-OPT-DAG: define linkonce_odr hidden i8* @swift_rt_swift_slowAlloc(i32, i32)
// CHECK-NO-OPT-DAG: define linkonce_odr hidden void @swift_rt_swift_release(%swift.refcounted*)
// CHECK-NO-OPT-DAG: define linkonce_odr hidden void @swift_rt_swift_retain(%swift.refcounted*)
// CHECK-NO-OPT-DAG: define linkonce_odr hidden void @swift_rt_swift_slowDealloc(i8*, i32, i32)

// CHECK-OPT-DAG: @_swift_retain = external dllimport local_unnamed_addr global void (%swift.refcounted*)
// CHECK-OPT-DAG: @_T0BoWV = external dllimport global i8*
// CHECK-OPT-DAG: @_T09dllexport1cCN = external dllimport global %swift.type
// CHECK-OPT-DAG: @_T09dllexport1pMp = external dllimport global %swift.protocol
// CHECK-OPT-DAG: @_swift_slowAlloc = external dllimport local_unnamed_addr global i8* (i32, i32)*
// CHECK-OPT-DAG: @_swift_slowDealloc = external dllimport local_unnamed_addr global void (i8*, i32, i32)*
// CHECK-OPT-DAG: declare dllimport swiftcc i8* @_T09dllexport2ciAA1cCfau()
// CHECK-OPT-DAG: declare dllimport %swift.type* @_T09dllexport1cCMa()
// CHECK-OPT-DAG: declare dllimport void @swift_deallocClassInstance(%swift.refcounted*, i32, i32)
// CHECK-OPT-DAG: declare dllimport swiftcc %swift.refcounted* @_T09dllexport1cCfd(%T9dllexport1cC* swiftself)
// CHECK-OPT-DAG: define linkonce_odr hidden i8* @swift_rt_swift_slowAlloc(i32, i32)
// CHECK-OPT-DAG: define linkonce_odr hidden void @swift_rt_swift_slowDealloc(i8*, i32, i32)

