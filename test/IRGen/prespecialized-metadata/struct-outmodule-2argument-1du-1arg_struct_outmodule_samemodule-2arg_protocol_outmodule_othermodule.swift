// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -prespecialize-generic-metadata -target %module-target-future %S/Inputs/struct-public-nonfrozen-1argument-1constraint.swift %S/Inputs/protocol-public-empty.swift %S/Inputs/struct-public-nonfrozen-0argument.swift -emit-library -o %t/%target-library-name(Module) -emit-module -module-name Module -emit-module-path %t/Module.swiftmodule
// RUN: %target-build-swift -Xfrontend -prespecialize-generic-metadata -target %module-target-future %S/Inputs/struct-public-nonfrozen-0argument-conformance-empty.swift -emit-library -o %t/%target-library-name(Argument) -emit-module -module-name Argument -emit-module-path %t/Argument.swiftmodule -DIMPORT_MODULE -L %t -I %t -lModule
// RUN: %swift -prespecialize-generic-metadata -target %module-target-future -emit-ir %s -L %t -I %t -lModule -lArgument | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment 

// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

// CHECK-NOT: @"$s6Module11OneArgumentVyAA7IntegerVAeA1P0C0yHCg_GMN" =

@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

import Module
import Argument

// CHECK: define hidden swiftcc void @"$s4main4doityyF"() #{{[0-9]+}} {
// CHECK:   [[METADATA:%[0-9]+]] = call %swift.type* @__swift_instantiateConcreteTypeFromMangledName({ i32, i32 }* @"$s6Module11OneArgumentVyAA7IntegerVAeA1P0C0yHCg_GMD")
// CHECK:   call swiftcc void @"$s4main7consumeyyxlF"(%swift.opaque* noalias nocapture {{%[0-9]+}}, %swift.type* [[METADATA]])
// CHECK: }
func doit() {
  consume( OneArgument(Integer(13)) )
}
doit()





