// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir | FileCheck %s

class Foo {
  func bar() {}
  @objc func baz() {}
  @IBAction func garply(_: AnyObject?) {}
}

// CHECK: @_INSTANCE_METHODS__TtC12objc_methods3Foo = private constant { {{.*}}] } {
// CHECK:   i32 24,
// CHECK:   i32 2,
// CHECK:   [2 x { i8*, i8*, i8* }] [{
// CHECK:     i8* getelementptr inbounds ([4 x i8]* @"\01L_selector_data(baz)", i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([8 x i8]* @0, i64 0, i64 0),
// CHECK:     i8* bitcast (void (i8*, i8*)* @_TToFC12objc_methods3Foo3bazfS0_FT_T_ to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([8 x i8]* @"\01L_selector_data(garply:)", i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([11 x i8]* @1, i64 0, i64 0),
// CHECK:     i8* bitcast (void (i8*, i8*, i8*)* @_TToFC12objc_methods3Foo6garplyfS0_FGSqPSs9AnyObject__T_ to i8*)
// CHECK:   }]
// CHECK: }, section "__DATA, __objc_const", align 8



// rdar://16006333 - observing properties don't work in @objc classes
@objc
class ObservingAccessorTest {
  var bounds: Int = 0 {
    willSet {}
    didSet {}
  }
}
