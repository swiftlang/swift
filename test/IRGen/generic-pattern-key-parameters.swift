// RUN: %target-swift-frontend -module-name main -emit-ir %s | %FileCheck %s

protocol P {
  associatedtype T
}
protocol Q {}

// CHECK: @_TMPV4main1A = {{.*}} i16 1,
struct A<T> {}
// CHECK: @_TMPV4main1B = {{.*}} i16 1,
struct B<T: AnyObject> {}
// CHECK: @_TMPV4main1C = {{.*}} i16 2,
struct C<T: P> {}
// CHECK: @_TMPV4main1D = {{.*}} i16 3,
struct D<T: P & Q> {}
// CHECK: @_TMPV4main1E = {{.*}} i16 2,
struct E<T: P & AnyObject> {}
// CHECK: @_TMPV4main1F = {{.*}} i16 3,
struct F<T: P & AnyObject & Q> {}
// CHECK: @_TMPV4main1G = {{.*}} i16 3,
struct G<T: P> where T.T: P {}
// CHECK: @_TMPV4main1H = {{.*}} i16 2,
struct H<T, U> {}
// CHECK: @_TMPV4main1I = {{.*}} i16 3,
struct I<T: P, U> {}
// CHECK: @_TMPV4main1J = {{.*}} i16 3,
struct J<T, U: P> {}
// CHECK: @_TMPV4main1K = {{.*}} i16 4,
struct K<T: P, U: P> {}
// CHECK: @_TMPV4main1L = {{.*}} i16 5,
struct L<T: P, U: P & Q>  {}
// CHECK: @_TMPV4main1M = {{.*}} i16 7,
struct M<T: P, U: P & Q> where T.T: P, U.T: Q {}

