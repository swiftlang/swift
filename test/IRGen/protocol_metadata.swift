// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

protocol A { func a() }
protocol B { func b() }
@class_protocol protocol C { func c() }
@objc, @class_protocol protocol O { func o() }

protocol AB : A, B { func ab() }
protocol ABO : A, B, O { func abo() }

// CHECK: @_TMp17protocol_metadata1A = constant %swift.protocol {
// -- size 72
// -- flags: 1 = Swift | 2 = Not Class-Constrained | 4 = Needs Witness Table
// CHECK:   i32 72, i32 7
// CHECK: }

// CHECK: @_TMp17protocol_metadata1B = constant %swift.protocol {
// CHECK:   i32 72, i32 7
// CHECK: }

// CHECK: @_TMp17protocol_metadata1C = constant %swift.protocol {
// -- flags: 1 = Swift | 4 = Needs Witness Table
// CHECK:   i32 72, i32 5
// CHECK: }

// -- @objc protocol O uses ObjC symbol mangling and layout
// CHECK: @_PROTOCOL_O = private constant { {{.*}} i32 } {
// CHECK:   @_PROTOCOL_INSTANCE_METHODS_O,
// -- flags: 1 = Swift
// CHECK:   i32 72, i32 1
// CHECK: }

// -- inheritance lists for refined protocols

// CHECK: [[AB_INHERITED:@.*]] = internal constant { {{.*}}* } {
// CHECK:   i64 2,
// CHECK:   %swift.protocol* @_TMp17protocol_metadata1A,
// CHECK:   %swift.protocol* @_TMp17protocol_metadata1B
// CHECK: }
// CHECK: @_TMp17protocol_metadata2AB = constant %swift.protocol { 
// CHECK:   [[AB_INHERITED]]
// CHECK:   i32 72, i32 7
// CHECK: }

// CHECK: [[ABO_INHERITED:@.*]] = internal constant { {{.*}}* } {
// CHECK:   i64 3,
// CHECK:   %swift.protocol* @_TMp17protocol_metadata1A,
// CHECK:   %swift.protocol* @_TMp17protocol_metadata1B,
// CHECK:   {{.*}}* @_PROTOCOL_O
// CHECK: }

/* TODO existential metadata
func reify_metadata<T>(x:T) {}

func protocol_types(a: A, b: B, c: C, o: O,
                    ab: protocol<A, B>,
                    ba: protocol<B, A>,
                    ac: protocol<A, C>,
                    abc: protocol<A, B, C>,
                    abco: protocol<A, B, C, O>,
                    co: protocol<C, O>) {
  reify_metadata(a)
  reify_metadata(b)
  reify_metadata(c)
  reify_metadata(o)
  reify_metadata(ab)
  reify_metadata(ba)
  reify_metadata(ac)
  reify_metadata(abc)
  reify_metadata(abco)
  reify_metadata(co)
}
*/
