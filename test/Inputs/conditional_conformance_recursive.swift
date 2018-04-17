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
// CHECK-LABEL: define internal void @"$S33conditional_conformance_recursive7WrapperVyxGAA2P3A2aERzrlWI"
// CHECK-NOT: ret
// CHECK: call i8** @"$S33conditional_conformance_recursive7WrapperVyxGAA2P2A2aERzrlWa"

// associated type metadata accessor for B in Wrapper<T>: P2
// CHECK-LABEL: define internal swiftcc %swift.metadata_response @"$S33conditional_conformance_recursive7WrapperVyxGAA2P2A2aERzrl1BWt"
// CHECK:   [[T_TO_P2_PTR:%.*]] = getelementptr inbounds i8*, i8** [[WRAPPER_T_TO_P2:%.*]], i32 -1
// CHECK:   [[T_TO_P2_VAL:%.*]] = load i8*, i8** [[T_TO_P2_PTR]]
// CHECK:   [[T_TO_P2:%.*]] = bitcast i8* [[T_TO_P2_VAL]] to i8**
// CHECK:   [[T_TO_P1_VAL_GEP:%.*]] = getelementptr inbounds i8*, i8** [[T_TO_P2]], i32 1
// CHECK:   [[T_TO_P1_VAL:%.*]] = load i8*, i8** [[T_TO_P1_VAL_GEP]]
// CHECK:   [[T_TO_P1:%.*]] = bitcast i8* [[T_TO_P1_VAL]] to i8**
// CHECK:   [[WRAPPER_T_TYPE:%.*]] = bitcast %swift.type* [[WRAPPER_T:%.*]] to %swift.type**
// CHECK:   [[T_TYPE_PTR:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[WRAPPER_T_TYPE]], i64 2
// CHECK:   [[T_TYPE:%.*]] = load %swift.type*, %swift.type** [[T_TYPE_PTR]]
// CHECK:   [[T_B_TYPE_ACCESSOR_PTR_GEP:%.*]] = getelementptr inbounds i8*, i8** [[T_TO_P1]], i32 1
// CHECK:   [[T_B_TYPE_ACCESSOR_PTR:%.*]] = load i8*, i8** [[T_B_TYPE_ACCESSOR_PTR_GEP]], align 8
// CHECK:   [[T_B_TYPE_ACCESSOR:%.*]] = bitcast i8* [[T_B_TYPE_ACCESSOR_PTR]] to %swift.metadata_response (i64, %swift.type*, i8**)*
// CHECK:   [[T_A_TYPE:%.*]] = call swiftcc %swift.metadata_response [[T_B_TYPE_ACCESSOR]](i64 %0, %swift.type* [[T_TYPE]], i8** [[T_TO_P1]])
// CHECK:   [[T0:%.*]] = extractvalue %swift.metadata_response [[T_A_TYPE]], 0
// CHECK:   [[T1:%.*]] = extractvalue %swift.metadata_response [[T_A_TYPE]], 1
// CHECK:   [[T2:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[T0]], 0
// CHECK:   [[T3:%.*]] = insertvalue %swift.metadata_response [[T2]], i64 [[T1]], 1
// CHECK:   ret %swift.metadata_response [[T3]]

// associated type witness table accessor for A : P2 in Wrapper<T>: P2
// CHECK-LABEL: define internal swiftcc i8** @"$S33conditional_conformance_recursive7WrapperVyxGAA2P2A2aERzrl1A_AaEPWT"
// CHECK: [[CONDITIONAL_REQ_BUFFER:%.*]] = alloca [1 x i8**]
// CHECK: [[FIRST_REQ:%.*]] = getelementptr inbounds [1 x i8**], [1 x i8**]* [[CONDITIONAL_REQ_BUFFER]]
// CHECK: call i8** @"$S33conditional_conformance_recursive7WrapperVyxGAA2P2A2aERzrlWa"(%swift.type* [[WRAPPER_TO_A:%.*]], i8*** [[FIRST_REQ]])
