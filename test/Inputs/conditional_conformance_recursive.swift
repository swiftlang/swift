protocol P1 {
  associatedtype A: P1
}

protocol P2: P1 where A: P2 {
  associatedtype B
}

struct Wrapper<T: P1>: P1 {
  typealias A = Wrapper<T>
}

extension Wrapper: P2 where T: P2 {
  typealias B = T.A
}

protocol P3: P2 where A: P3 { }

extension Wrapper: P3 where T: P3 { }

// instantiation function for Wrapper<T>: P3
// CHECK-LABEL: define internal void @"$s33conditional_conformance_recursive7WrapperVyxGAA2P3A2aERzrlWI"
// CHECK-NOT: ret
// CHECK: call i8** @swift_getWitnessTable

// associated type witness table accessor for A : P2 in Wrapper<T>: P2
// CHECK-LABEL: define internal swiftcc i8** @"$s33conditional_conformance_recursive7WrapperVyxGAA2P2A2aERzrl1A_AaEPWT"
// CHECK: [[CONDITIONAL_REQ_BUFFER:%.*]] = alloca [1 x i8**]
// CHECK: [[FIRST_REQ:%.*]] = getelementptr inbounds [1 x i8**], [1 x i8**]* [[CONDITIONAL_REQ_BUFFER]]
// CHECK: call i8** @swift_getWitnessTable
