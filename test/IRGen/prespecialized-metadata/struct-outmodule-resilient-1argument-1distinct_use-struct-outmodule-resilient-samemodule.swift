// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -prespecialize-generic-metadata -target %module-target-future %S/Inputs/struct-public-nonfrozen-1argument.swift %S/Inputs/struct-public-nonfrozen-0argument.swift -emit-library -o %t/%target-library-name(Generic) -emit-module -module-name Generic -emit-module-path %t/Generic.swiftmodule -enable-library-evolution
// RUN: %swift -prespecialize-generic-metadata -target %module-target-future -emit-ir %s -L %t -I %t -lGeneric | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment 

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

// CHECK-NOT: @"$s7Generic11OneArgumentVyAA7IntegerVGMN" =

@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

import Generic

// CHECK: define hidden swiftcc void @"$s4main4doityyF"() #{{[0-9]+}} {
// CHECK:   [[METADATA:%[0-9]+]] = call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$s7Generic11OneArgumentVyAA7IntegerVGMD")
// CHECK:   call swiftcc void @"$s4main7consumeyyxlF"(ptr noalias {{%[0-9]+}}, ptr [[METADATA]])
// CHECK: }
func doit() {
  consume( OneArgument(Integer(13)) )
}
doit()

