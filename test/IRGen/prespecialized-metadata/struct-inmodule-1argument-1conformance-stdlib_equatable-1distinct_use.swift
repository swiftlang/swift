// RUN: %swift -target %module-target-future -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

// CHECK-NOT: @"$s4main5ValueVyAA7IntegerVGMf"
struct Value<First : Equatable> {
  let first: First
}

struct Integer : Equatable {
  let value: Int
}

@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

func doit() {
  consume( Value(first: Integer(value: 13)) )
}
doit()

// CHECK: ; Function Attrs: noinline nounwind memory(none)
// CHECK: define hidden swiftcc %swift.metadata_response @"$s4main5ValueVMa"([[INT]] %0, ptr %1, ptr %2) #{{[0-9]+}} {{(section)?.*}}{
// CHECK: entry:
// CHECK:   {{%[0-9]+}} = call swiftcc %swift.metadata_response @__swift_instantiateGenericMetadata(
// CHECK-SAME:   [[INT]] %0, 
// CHECK-SAME:   ptr %1, 
// CHECK-SAME:   ptr %2, 
// CHECK-SAME:   ptr undef, 
// CHECK-SAME:   $s4main5ValueVMn
// CHECK:   ret %swift.metadata_response {{%[0-9]+}}
// CHECK: }
