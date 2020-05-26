// RUN: %swift -target thumbv7--windows-itanium -emit-ir -parse-as-library -disable-legacy-type-info -parse-stdlib -module-name dllimport %s -o - -enable-source-import -I %S | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-NO-OPT
// RUN: %swift -target thumbv7--windows-itanium -O -emit-ir -parse-as-library -disable-legacy-type-info -parse-stdlib -module-name dllimport -primary-file %s -o - -enable-source-import -I %S | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-OPT

// REQUIRES: CODEGENERATOR=ARM

import dllexport

public func get_ci() -> dllexport.c {
  return dllexport.ci
}

public func get_c_type() -> dllexport.c.Type {
  return dllexport.c.self
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

// CHECK-NO-OPT-DAG: declare dllimport %swift.refcounted* @swift_allocObject(%swift.type*, i32, i32)
// CHECK-NO-OPT-DAG: declare dllimport void @swift_deallocObject(%swift.refcounted*, i32, i32)
// CHECK-NO-OPT-DAG: declare dllimport void @swift_release(%swift.refcounted*)
// CHECK-NO-OPT-DAG: declare dllimport %swift.refcounted* @swift_retain(%swift.refcounted* returned)
// CHECK-NO-OPT-DAG: @"$s9dllexport1pMp" = external dllimport global %swift.protocol
// CHECK-NO-OPT-DAG: declare dllimport swiftcc i8* @"$s9dllexport2ciAA1cCvau"()
// CHECK-NO-OPT-DAG: declare dllimport swiftcc %swift.refcounted* @"$s9dllexport1cCfd"(%T9dllexport1cC* swiftself)
// CHECK-NO-OPT-DAG: declare dllimport void @swift_deallocClassInstance(%swift.refcounted*, i32, i32)

// CHECK-OPT-DAG: declare dllimport %swift.refcounted* @swift_retain(%swift.refcounted* returned) local_unnamed_addr
// CHECK-OPT-DAG: @"__imp_$s9dllexport1pMp" = external externally_initialized constant %swift.protocol*
// CHECK-OPT-DAG: declare dllimport swiftcc i8* @"$s9dllexport2ciAA1cCvau"()
// CHECK-OPT-DAG: declare dllimport void @swift_deallocClassInstance(%swift.refcounted*, i32, i32)
// CHECK-OPT-DAG: declare dllimport swiftcc %swift.refcounted* @"$s9dllexport1cCfd"(%T9dllexport1cC* swiftself)
