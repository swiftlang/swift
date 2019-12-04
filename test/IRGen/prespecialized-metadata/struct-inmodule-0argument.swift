// RUN: %swift -target %module-target-future -emit-ir -prespecialize-generic-metadata %s | %FileCheck %s

struct Value {
  let first: Int
}

@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

// CHECK: define hidden swiftcc void @"$s4main4doityyF"() #0 {
// CHECK:   call swiftcc void @"$s4main7consumeyyxlF"(%swift.opaque* noalias nocapture %{{[0-9]+}}, %swift.type* bitcast (i64* getelementptr inbounds (<{ i8**, i64, <{ i32, i32, i32, i32, i32, i32, i32 }>*, i32, [4 x i8], i64 }>, <{ i8**, i64, <{ i32, i32, i32, i32, i32, i32, i32 }>*, i32, [4 x i8], i64 }>* @"$s4main5ValueVMf", i32 0, i32 1) to %swift.type*))
// CHECK: }
func doit() {
  consume( Value(first: 13) )
}
doit()
