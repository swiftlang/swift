// RUN: %swift -target %module-target-future -emit-ir -prespecialize-generic-metadata %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment

// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

// CHECK-NOT: @"$s4main5ValueVyAA7IntegerVGMf"
struct Value<First : Hashable> {
  let first: First
}

struct Integer : Hashable {
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

// CHECK: ; Function Attrs: noinline nounwind readnone
// CHECK: define hidden swiftcc %swift.metadata_response @"$s4main5ValueVMa"([[INT]], %swift.type*, i8**) #{{[0-9]+}} {
// CHECK: entry:
// CHECK:   [[ERASED_TYPE:%[0-9]+]] = bitcast %swift.type* %1 to i8*
// CHECK:   [[ERASED_CONFORMANCE:%[0-9]+]] = bitcast i8** %2 to i8*
// CHECK:   {{%[0-9]+}} = call swiftcc %swift.metadata_response @__swift_instantiateGenericMetadata([[INT]] %0, i8* [[ERASED_TYPE]], i8* [[ERASED_CONFORMANCE]], i8* undef, %swift.type_descriptor* bitcast (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i16, i16, i8, i8, i8, i8, i32, i32, i32 }>* @"$s4main5ValueVMn" to %swift.type_descriptor*)) #{{[0-9]+}}
// CHECK:   ret %swift.metadata_response {{%[0-9]+}}
// CHECK: }
