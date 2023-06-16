// RUN: %swift %use_no_opaque_pointers -prespecialize-generic-metadata -target %module-target-future -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment
// RUN: %swift -prespecialize-generic-metadata -target %module-target-future -emit-ir %s

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

struct Value {
  let first: Int
}

@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

// CHECK: define hidden swiftcc void @"$s4main4doityyF"() #{{[0-9]+}} {
// CHECK:   call swiftcc void @"$s4main7consumeyyxlF"(%swift.opaque* noalias nocapture %{{[0-9]+}}, %swift.type* bitcast ([[INT]]* getelementptr inbounds (<{ i8*, i8**, [[INT]], <{ i32, i32, i32, i32, i32, i32, i32 }>*, i32{{(, \[4 x i8\])?}}, i64 }>, <{ i8*, i8**, [[INT]], <{ i32, i32, i32, i32, i32, i32, i32 }>*, i32{{(, \[4 x i8\])?}}, i64 }>* @"$s4main5ValueVMf", i32 0, i32 2) to %swift.type*))
// CHECK: }
func doit() {
  consume( Value(first: 13) )
}
doit()
