// RUN: %target-swift-frontend %s -emit-ir -disable-objc-attr-requires-foundation-module | FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

// Protocol methods require extended method type encodings to capture block
// signatures.
@objc protocol Fooable {
  func block(_: Int -> Int)
  func block2(_: (Int,Int) -> Int)
}

class Foo: Fooable {
  func bar() {}
  @objc func baz() {}
  @IBAction func garply(_: AnyObject?) {}
  @objc func block(_: Int -> Int) {}
  @objc func block2(_: (Int,Int) -> Int) {}
}

// CHECK: [[BLOCK_SIGNATURE_TRAD:@.*]] = private unnamed_addr constant [12 x i8] c"v24@0:8@?16\00"
// CHECK: [[BLOCK_SIGNATURE_EXT_1:@.*]] = private unnamed_addr constant [18 x i8] c"v24@0:8@?<q@?q>16\00"
// CHECK: [[BLOCK_SIGNATURE_EXT_2:@.*]] = private unnamed_addr constant [19 x i8] c"v24@0:8@?<q@?qq>16\00"
// CHECK: @_PROTOCOL_METHOD_TYPES__TtP12objc_methods7Fooable_ = private constant { [2 x i8*] } {
// CHECK:   [2 x i8*] [
// CHECK:     i8* getelementptr inbounds ([18 x i8], [18 x i8]* [[BLOCK_SIGNATURE_EXT_1]], i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([19 x i8], [19 x i8]* [[BLOCK_SIGNATURE_EXT_2]], i64 0, i64 0)
// CHECK:   ]
// CHECK: }
// CHECK: [[BAZ_SIGNATURE:@.*]] = private unnamed_addr constant [8 x i8] c"v16@0:8\00"
// CHECK: [[GARPLY_SIGNATURE:@.*]] = private unnamed_addr constant [11 x i8] c"v24@0:8@16\00"
// CHECK: @_INSTANCE_METHODS__TtC12objc_methods3Foo = private constant { {{.*}}] } {
// CHECK:   i32 24,
// CHECK:   i32 4,
// CHECK:   [4 x { i8*, i8*, i8* }] [{
// CHECK:     i8* getelementptr inbounds ([4 x i8], [4 x i8]* @"\01L_selector_data(baz)", i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([8 x i8], [8 x i8]* [[BAZ_SIGNATURE]], i64 0, i64 0),
// CHECK:     i8* bitcast (void (i8*, i8*)* @_TToFC12objc_methods3Foo3bazfS0_FT_T_ to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([8 x i8], [8 x i8]* @"\01L_selector_data(garply:)", i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([11 x i8], [11 x i8]* [[GARPLY_SIGNATURE]], i64 0, i64 0),
// CHECK:     i8* bitcast (void (i8*, i8*, i8*)* @_TToFC12objc_methods3Foo6garplyfS0_FGSqPSs9AnyObject__T_ to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([7 x i8], [7 x i8]* @"\01L_selector_data(block:)", i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([12 x i8], [12 x i8]* [[BLOCK_SIGNATURE_TRAD]], i64 0, i64 0),
// CHECK:     i8* bitcast (void (i8*, i8*, i64 (i64)*)* @_TToFC12objc_methods3Foo5blockfS0_FFSiSiT_ to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([8 x i8], [8 x i8]* @"\01L_selector_data(block2:)", i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([12 x i8], [12 x i8]* [[BLOCK_SIGNATURE_TRAD]], i64 0, i64 0),
// CHECK:     i8* bitcast (void (i8*, i8*, i64 (i64, i64)*)* @_TToFC12objc_methods3Foo6block2fS0_FFTSiSi_SiT_ to i8*)
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
