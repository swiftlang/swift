// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -prespecialize-generic-metadata -target %module-target-future %S/Inputs/struct-public-frozen-1argument.swift -emit-library -o %t/%target-library-name(Generic) -emit-module -module-name Generic -emit-module-path %t/Generic.swiftmodule -enable-library-evolution
// RUN: %target-build-swift -Xfrontend -prespecialize-generic-metadata -target %module-target-future %S/Inputs/struct-public-nonfrozen-0argument.swift -emit-library -o %t/%target-library-name(Argument) -emit-module -module-name Argument -emit-module-path %t/Argument.swiftmodule -enable-library-evolution
// RUN: %swift %use_no_opaque_pointers -prespecialize-generic-metadata -target %module-target-future -emit-ir %s -L %t -I %t -lGeneric -lArgument | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment 
// RUN: %swift -prespecialize-generic-metadata -target %module-target-future -emit-ir %s -L %t -I %t -lGeneric -lArgument

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

// CHECK-NOT: @"$s7Generic11OneArgumentVy0C07IntegerVGMN" =

@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

import Generic
import Argument

// CHECK: define hidden swiftcc void @"$s4main4doityyF"() #{{[0-9]+}} {
// CHECK:   [[METADATA:%[0-9]+]] = call %swift.type* @__swift_instantiateConcreteTypeFromMangledName({ i32, i32 }* @"$s7Generic11OneArgumentVy0C07IntegerVGMD")
// CHECK:   call swiftcc void @"$s4main7consumeyyxlF"(%swift.opaque* noalias nocapture {{%[0-9]+}}, %swift.type* [[METADATA]])
// CHECK: }
func doit() {
  consume( OneArgument(Integer(13)) )
}
doit()




