// RUN: %swift -prespecialize-generic-metadata -target %module-target-future -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

// CHECK: @"$s4main9NamespaceC5ValueVySi_GMf" = linkonce_odr hidden constant <{
// CHECk-SAME:    ptr,
// CHECK-SAME:    [[INT]],
// CHECK-SAME:    ptr,
// CHECK-SAME:    ptr,
// CHECK-SAME:    i32, 
// CHECK-SAME:    i64
// CHECK-SAME: }> <{
//                ptr @"$sB[[INT]]_WV",
//                ptr {{[^@]*}}@"$s4main9NamespaceC5ValueVySi_GWV"{{[^,]*}},
// CHECK-SAME:    [[INT]] 512,
// CHECK-SAME:    $s4main9NamespaceC5ValueVMn
// CHECK-SAME:    $sSiN
// CHECK-SAME:    i32 0,
// CHECK-SAME:    i64 3
// CHECK-SAME: }>, align [[ALIGNMENT]]


final class Namespace<Arg> {
  struct Value {
    let first: Arg
  }
}

@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

// CHECK: define hidden swiftcc void @"$s4main4doityyF"() #{{[0-9]+}} {
// CHECK:   call swiftcc void @"$s4main7consumeyyxlF"(
// CHECK-SAME:   ptr noalias %{{[0-9]+}}, 
// CHECK-SAME:   ptr getelementptr inbounds (
// CHECK-SAME:     %swift.full_type, 
// CHECK-SAME:     $s4main9NamespaceC5ValueVySi_GMf
// CHECK-SAME:     i32 0, 
// CHECK-SAME:     i32 2
// CHECK-SAME:   )
// CHECK-SAME: )
// CHECK: }
func doit() {
  consume( Namespace.Value(first: 13) )
}
doit()

// CHECK: ; Function Attrs: noinline nounwind memory(none)
// CHECK: define hidden swiftcc %swift.metadata_response @"$s4main9NamespaceC5ValueVMa"([[INT]] %0, ptr %1) #{{[0-9]+}} {{(section)?.*}}{
// CHECK: entry:
// CHECK:      call swiftcc %swift.metadata_response @__swift_instantiateCanonicalPrespecializedGenericMetadata(
// CHECK-SAME:   [[INT]] %0, 
// CHECK-SAME:   ptr %1, 
// CHECK-SAME:   ptr undef, 
// CHECK-SAME:   ptr undef, 
// CHECK-SAME:   ptr {{.+}}$s4main9NamespaceC5ValueVMn{{.+}}, 
// CHECK-SAME:   $s4main9NamespaceC5ValueVMz
// CHECK:   ret %swift.metadata_response {{%[0-9]+}}
// CHECK: }
