// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// <rdar://problem/13793646>
struct OptionalEnumeratorAdaptor<T: Enumerator> {
  // CHECK: define { i8*, i64, %swift.refcounted* } @_TV15generic_ternary25OptionalEnumeratorAdaptor4nextUSs10Enumerator___fRGS0_Q__FT_GVSs8OptionalQ0__(%V15generic_ternary25OptionalEnumeratorAdaptor*, %swift.type* %This)
  func next() -> Optional<T.Element> {
    return x[0].isEmpty() ? +None : Some(x[0].next())
  }
  var x: T[]
}
