// RUN: %target-swift-frontend -module-name keypaths -emit-ir %s | %FileCheck %s

// The purpose of this test is to validate that a keypath formed via a protocol
// that has an inverse requirement on Self produces the same generic environment
// metadata as one without the inverse. So Mashable and Dashable should have the
// same metadata fundamentally.

// CHECK-LABEL: @"generic environment 8keypaths8MashableRzl" = linkonce_odr hidden constant
// CHECK-SAME:    i32 4097, i16 1, i8 -128, [1 x i8] zeroinitializer, i32 128

// CHECK-LABEL: @"generic environment 8keypaths8DashableRzl" = linkonce_odr hidden constant
// CHECK-SAME:    i32 4097, i16 1, i8 -128, [1 x i8] zeroinitializer, i32 128


protocol Mashable: ~Copyable {
  var masher: String { get set }
}

protocol Dashable {
  var dasher: String { get set }
}

func formKeypath1<T: Mashable>(_ t: T) -> WritableKeyPath<T, String> {
  return \T.masher
}

func formKeypath2<T: Dashable>(_ t: T) -> WritableKeyPath<T, String> {
  return \T.dasher
}
