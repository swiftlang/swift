public func takes_p1<T: P1>(_: T.Type) {}
public protocol P1 {
  func normal()
  func generic<T: P3>(_: T)
}
public protocol P2 {}
public protocol P3 {}

public struct IsP2: P2 {}
public struct IsP3: P3 {}


public struct Single<A> {}
extension Single: P1 where A: P2 {
  public func normal() {}
  public func generic<T: P3>(_: T) {}
}

// witness method for Single.normal

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @_T042conditional_conformance_basic_conformances6SingleVyxGAA2P1A2A2P2RzlAaEP6normalyyFTW(%T42conditional_conformance_basic_conformances6SingleV* noalias nocapture swiftself, %swift.type* %Self, i8** %SelfWitnessTable) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[A_P2_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -1
// CHECK-NEXT:    [[A_P2_i8star:%.*]] = load i8*, i8** [[A_P2_PTR]], align 8
// CHECK-NEXT:    [[A_P2:%.*]] = bitcast i8* [[A_P2_i8star]] to i8**
// CHECK-NEXT:    [[SELF_AS_TYPE_ARRAY:%.*]] = bitcast %swift.type* %Self to %swift.type**
// CHECK-NEXT:    [[A_PTR:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[SELF_AS_TYPE_ARRAY]], i64 2
// CHECK-NEXT:    [[A:%.*]] = load %swift.type*, %swift.type** [[A_PTR]], align 8
// CHECK-NEXT:    call swiftcc void @_T042conditional_conformance_basic_conformances6SingleVA2A2P2RzlE6normalyyF(%swift.type* [[A]], i8** [[A_P2]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// witness method for Single.generic

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @_T042conditional_conformance_basic_conformances6SingleVyxGAA2P1A2A2P2RzlAaEP7genericyqd__AA2P3Rd__lFTW(%swift.opaque* noalias nocapture, %swift.type* %"\CF\84_1_0", i8** %"\CF\84_1_0.P3", %T42conditional_conformance_basic_conformances6SingleV* noalias nocapture swiftself, %swift.type* %Self, i8** %SelfWitnessTable) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[A_P2_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -1
// CHECK-NEXT:    [[A_P2_i8star:%.*]] = load i8*, i8** [[A_P2_PTR]], align 8
// CHECK-NEXT:    [[A_P2:%.*]] = bitcast i8* [[A_P2_i8star]] to i8**
// CHECK-NEXT:    [[SELF_AS_TYPE_ARRAY:%.*]] = bitcast %swift.type* %Self to %swift.type**
// CHECK-NEXT:    [[A_PTR:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[SELF_AS_TYPE_ARRAY]], i64 2
// CHECK-NEXT:    [[A:%.*]] = load %swift.type*, %swift.type** [[A_PTR]], align 8
// CHECK-NEXT:    call swiftcc void @_T042conditional_conformance_basic_conformances6SingleVA2A2P2RzlE7genericyqd__AA2P3Rd__lF(%swift.opaque* noalias nocapture %0, %swift.type* [[A]], %swift.type* %"\CF\84_1_0", i8** [[A_P2]], i8** %"\CF\84_1_0.P3")
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

public func single_generic<T: P2>(_: T.Type) {
  takes_p1(Single<T>.self)
}
// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T042conditional_conformance_basic_conformances14single_genericyxmAA2P2RzlF(%swift.type*, %swift.type* %T, i8** %T.P2) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [1 x i8**], align 8
// CHECK-NEXT:    [[Single_TYPE:%.*]] = call %swift.type* @_T042conditional_conformance_basic_conformances6SingleVMa(%swift.type* %T)
// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [1 x i8**], [1 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[T_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** %T.P2, i8*** [[T_P2_PTR]], align 8
// CHECK-NEXT:    [[Single_P1:%.*]] = call i8** @_T042conditional_conformance_basic_conformances6SingleVyxGAA2P1A2A2P2RzlWa(%swift.type* [[Single_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]], i64 1)
// CHECK-NEXT:    call swiftcc void @_T042conditional_conformance_basic_conformances8takes_p1yxmAA2P1RzlF(%swift.type* [[Single_TYPE]], %swift.type* [[Single_TYPE]], i8** [[Single_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// Witness table accessor for Single : P1

// CHECK-LABEL: define{{( protected)?}} i8** @_T042conditional_conformance_basic_conformances6SingleVyxGAA2P1A2A2P2RzlWa(%swift.type*, i8***, i64) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.tables = alloca %swift.witness_table_slice, align 8
// CHECK-NEXT:    [[TABLES_PTR:%.*]] = getelementptr inbounds %swift.witness_table_slice, %swift.witness_table_slice* %conditional.tables, i32 0, i32 0
// CHECK-NEXT:    store i8*** %1, i8**** [[TABLES_PTR]], align 8
// CHECK-NEXT:    [[COUNT_PTR:%.*]] = getelementptr inbounds %swift.witness_table_slice, %swift.witness_table_slice* %conditional.tables, i32 0, i32 1
// CHECK-NEXT:    store i64 %2, i64* [[COUNT_PTR]], align 8
// CHECK-NEXT:    [[CAST_CONDITIONAL_TABLES:%.*]] = bitcast %swift.witness_table_slice* %conditional.tables to i8**
// CHECK-NEXT:    [[TABLE:%.*]] = call i8** @swift_rt_swift_getGenericWitnessTable(%swift.generic_witness_table_cache* @_T042conditional_conformance_basic_conformances6SingleVyxGAA2P1A2A2P2RzlWG, %swift.type* %0, i8** [[CAST_CONDITIONAL_TABLES]])
// CHECK-NEXT:    ret i8** [[TABLE]]
// CHECK-NEXT:  }


public func single_concrete() {
  takes_p1(Single<IsP2>.self)
}
// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T042conditional_conformance_basic_conformances15single_concreteyyF() #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[Single_TYPE:%.*]] = call %swift.type* @_T042conditional_conformance_basic_conformances6SingleVyAA4IsP2VGMa()
// CHECK-NEXT:    [[Single_P1:%.*]] = call i8** @_T042conditional_conformance_basic_conformances6SingleVyAA4IsP2VGACyxGAA2P1A2A0G0RzlWl()
// CHECK-NEXT:    call swiftcc void @_T042conditional_conformance_basic_conformances8takes_p1yxmAA2P1RzlF(%swift.type* [[Single_TYPE]], %swift.type* [[Single_TYPE]], i8** [[Single_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }


// Lazy witness table accessor for the concrete Single<IsP2> : P1.

// CHECK-LABEL: define linkonce_odr hidden i8** @_T042conditional_conformance_basic_conformances6SingleVyAA4IsP2VGACyxGAA2P1A2A0G0RzlWl() #1 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [1 x i8**], align 8
// CHECK-NEXT:    [[CACHE:%.*]] = load i8**, i8*** @_T042conditional_conformance_basic_conformances6SingleVyAA4IsP2VGACyxGAA2P1A2A0G0RzlWL, align 8
// CHECK-NEXT:    [[IS_NULL:%.*]] = icmp eq i8** [[CACHE]], null
// CHECK-NEXT:    br i1 [[IS_NULL]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:    [[Single_TYPE:%.*]] = call %swift.type* @_T042conditional_conformance_basic_conformances6SingleVyAA4IsP2VGMa()

// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [1 x i8**], [1 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[A_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** getelementptr inbounds ([0 x i8*], [0 x i8*]* @_T042conditional_conformance_basic_conformances4IsP2VAA0F0AAWP, i32 0, i32 0), i8*** [[A_P2_PTR]], align 8

// CHECK-NEXT:    [[Single_P1:%.*]] = call i8** @_T042conditional_conformance_basic_conformances6SingleVyxGAA2P1A2A2P2RzlWa(%swift.type* [[Single_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]], i64 1) [[ATTRS:#[0-9]+]]
// CHECK-NEXT:    store atomic i8** [[Single_P1]], i8*** @_T042conditional_conformance_basic_conformances6SingleVyAA4IsP2VGACyxGAA2P1A2A0G0RzlWL release, align 8
// CHECK-NEXT:    br label %cont

// CHECK:       cont:
// CHECK-NEXT:    %6 = phi i8** [ [[CACHE]], %entry ], [ [[Single_P1]], %cacheIsNull ]
// CHECK-NEXT:    ret i8** %6
// CHECK-NEXT:  }


public struct Double<B, C> {}
extension Double: P1 where B: P2, C: P3 {
  public func normal() {}
  public func generic<T: P3>(_: T) {}
}

// witness method for Double.normal

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @_T042conditional_conformance_basic_conformances6DoubleVyxq_GAA2P1A2A2P2RzAA2P3R_rlAaEP6normalyyFTW(%T42conditional_conformance_basic_conformances6DoubleV* noalias nocapture swiftself, %swift.type* %Self, i8** %SelfWitnessTable) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[B_P2_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -1
// CHECK-NEXT:    [[B_P2_i8star:%.*]] = load i8*, i8** [[B_P2_PTR]], align 8
// CHECK-NEXT:    [[B_P2:%.*]] = bitcast i8* [[B_P2_i8star]] to i8**

// CHECK-NEXT:    [[C_P3_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -2
// CHECK-NEXT:    [[C_P3_i8star:%.*]] = load i8*, i8** [[C_P3_PTR]], align 8
// CHECK-NEXT:    [[C_P3:%.*]] = bitcast i8* [[C_P3_i8star]] to i8**

// CHECK-NEXT:    [[SELF_AS_TYPE_ARRAY:%.*]] = bitcast %swift.type* %Self to %swift.type**
// CHECK-NEXT:    [[B_PTR:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[SELF_AS_TYPE_ARRAY]], i64 2
// CHECK-NEXT:    [[B:%.*]] = load %swift.type*, %swift.type** [[B_PTR]], align 8

// CHECK-NEXT:    [[SELF_AS_TYPE_ARRAY_2:%.*]] = bitcast %swift.type* %Self to %swift.type**
// CHECK-NEXT:    [[C_PTR:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[SELF_AS_TYPE_ARRAY_2]], i64 3
// CHECK-NEXT:    [[C:%.*]] = load %swift.type*, %swift.type** [[C_PTR]], align 8

// CHECK-NEXT:    call swiftcc void @_T042conditional_conformance_basic_conformances6DoubleVA2A2P2RzAA2P3R_rlE6normalyyF(%swift.type* [[B]], %swift.type* [[C]], i8** [[B_P2]], i8** [[C_P3]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// witness method for Double.generic

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @_T042conditional_conformance_basic_conformances6DoubleVyxq_GAA2P1A2A2P2RzAA2P3R_rlAaEP7genericyqd__AaGRd__lFTW(%swift.opaque* noalias nocapture, %swift.type* %"\CF\84_1_0", i8** %"\CF\84_1_0.P3", %T42conditional_conformance_basic_conformances6DoubleV* noalias nocapture swiftself, %swift.type* %Self, i8** %SelfWitnessTable) #0 {
// CHECK-NEXT:  entry:

// CHECK-NEXT:    [[B_P2_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -1
// CHECK-NEXT:    [[B_P2_i8star:%.*]] = load i8*, i8** [[B_P2_PTR]], align 8
// CHECK-NEXT:    [[B_P2:%.*]] = bitcast i8* [[B_P2_i8star]] to i8**

// CHECK-NEXT:    [[C_P3_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -2
// CHECK-NEXT:    [[C_P3_i8star:%.*]] = load i8*, i8** [[C_P3_PTR]], align 8
// CHECK-NEXT:    [[C_P3:%.*]] = bitcast i8* [[C_P3_i8star]] to i8**

// CHECK-NEXT:    [[SELF_AS_TYPE_ARRAY:%.*]] = bitcast %swift.type* %Self to %swift.type**
// CHECK-NEXT:    [[B_PTR:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[SELF_AS_TYPE_ARRAY]], i64 2
// CHECK-NEXT:    [[B:%.*]] = load %swift.type*, %swift.type** [[B_PTR]], align 8

// CHECK-NEXT:    [[SELF_AS_TYPE_ARRAY_2:%.*]] = bitcast %swift.type* %Self to %swift.type**
// CHECK-NEXT:    [[C_PTR:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[SELF_AS_TYPE_ARRAY_2]], i64 3
// CHECK-NEXT:    [[C:%.*]] = load %swift.type*, %swift.type** [[C_PTR]], align 8

// CHECK-NEXT:    call swiftcc void @_T042conditional_conformance_basic_conformances6DoubleVA2A2P2RzAA2P3R_rlE7genericyqd__AaERd__lF(%swift.opaque* noalias nocapture %0, %swift.type* [[B]], %swift.type* [[C]], %swift.type* %"\CF\84_1_0", i8** [[B_P2]], i8** [[C_P3]], i8** %"\CF\84_1_0.P3")
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }


public func double_generic_generic<U: P2, V: P3>(_: U.Type, _: V.Type) {
  takes_p1(Double<U, V>.self)
}
// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T042conditional_conformance_basic_conformances015double_generic_F0yxm_q_mtAA2P2RzAA2P3R_r0_lF(%swift.type*, %swift.type*, %swift.type* %U, %swift.type* %V, i8** %U.P2, i8** %V.P3) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [2 x i8**], align 8
// CHECK-NEXT:    [[Double_TYPE:%.*]] = call %swift.type* @_T042conditional_conformance_basic_conformances6DoubleVMa(%swift.type* %U, %swift.type* %V)

// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [2 x i8**], [2 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[B_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** %U.P2, i8*** [[B_P2_PTR]], align 8
// CHECK-NEXT:    [[C_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 1
// CHECK-NEXT:    store i8** %V.P3, i8*** [[C_P3_PTR]], align 8

// CHECK-NEXT:    [[Double_P1:%.*]] = call i8** @_T042conditional_conformance_basic_conformances6DoubleVyxq_GAA2P1A2A2P2RzAA2P3R_rlWa(%swift.type* [[Double_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]], i64 2)
// CHECK-NEXT:    call swiftcc void @_T042conditional_conformance_basic_conformances8takes_p1yxmAA2P1RzlF(%swift.type* [[Double_TYPE]], %swift.type* [[Double_TYPE]], i8** [[Double_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// witness table accessor for Double : P1

// CHECK-LABEL: define{{( protected)?}} i8** @_T042conditional_conformance_basic_conformances6DoubleVyxq_GAA2P1A2A2P2RzAA2P3R_rlWa(%swift.type*, i8***, i64) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.tables = alloca %swift.witness_table_slice, align 8
// CHECK-NEXT:    [[TABLES_PTR:%.*]] = getelementptr inbounds %swift.witness_table_slice, %swift.witness_table_slice* %conditional.tables, i32 0, i32 0
// CHECK-NEXT:    store i8*** %1, i8**** [[TABLES_PTR]], align 8
// CHECK-NEXT:    [[COUNT_PTR:%.*]] = getelementptr inbounds %swift.witness_table_slice, %swift.witness_table_slice* %conditional.tables, i32 0, i32 1
// CHECK-NEXT:    store i64 %2, i64* [[COUNT_PTR]], align 8
// CHECK-NEXT:    [[CAST_CONDITIONAL_TABLES:%.*]] = bitcast %swift.witness_table_slice* %conditional.tables to i8**
// CHECK-NEXT:    [[TABLE:%.*]] = call i8** @swift_rt_swift_getGenericWitnessTable(%swift.generic_witness_table_cache* @_T042conditional_conformance_basic_conformances6DoubleVyxq_GAA2P1A2A2P2RzAA2P3R_rlWG, %swift.type* %0, i8** [[CAST_CONDITIONAL_TABLES]])
// CHECK-NEXT:    ret i8** [[TABLE]]
// CHECK-NEXT:  }


public func double_generic_concrete<X: P2>(_: X.Type) {
  takes_p1(Double<X, IsP3>.self)
}
// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T042conditional_conformance_basic_conformances23double_generic_concreteyxmAA2P2RzlF(%swift.type*, %swift.type* %X, i8** %X.P2) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [2 x i8**], align 8
// CHECK-NEXT:    [[Double_TYPE:%.*]] = call %swift.type* @_T042conditional_conformance_basic_conformances6DoubleVMa(%swift.type* %X, %swift.type* bitcast (i64* getelementptr inbounds (<{ i8**, i64, <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i32 }>* }>, <{ i8**, i64, <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i32 }>* }>* @_T042conditional_conformance_basic_conformances4IsP3VMf, i32 0, i32 1) to %swift.type*))

// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [2 x i8**], [2 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[B_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** %X.P2, i8*** [[B_P2_PTR]], align 8
// CHECK-NEXT:    [[C_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 1
// CHECK-NEXT:    store i8** getelementptr inbounds ([0 x i8*], [0 x i8*]* @_T042conditional_conformance_basic_conformances4IsP3VAA0F0AAWP, i32 0, i32 0), i8*** [[C_P3_PTR]], align 8

// CHECK-NEXT:    [[Double_P1:%.*]] = call i8** @_T042conditional_conformance_basic_conformances6DoubleVyxq_GAA2P1A2A2P2RzAA2P3R_rlWa(%swift.type* %1, i8*** [[CONDITIONAL_REQUIREMENTS]], i64 2)
// CHECK-NEXT:    call swiftcc void @_T042conditional_conformance_basic_conformances8takes_p1yxmAA2P1RzlF(%swift.type* [[Double_TYPE]], %swift.type* [[Double_TYPE]], i8** [[Double_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

public func double_concrete_concrete() {
  takes_p1(Double<IsP2, IsP3>.self)
}
// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T042conditional_conformance_basic_conformances016double_concrete_F0yyF() #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[Double_TYPE:%.*]] = call %swift.type* @_T042conditional_conformance_basic_conformances6DoubleVyAA4IsP2VAA0F2P3VGMa()
// CHECK-NEXT:    [[Double_P1:%.*]] = call i8** @_T042conditional_conformance_basic_conformances6DoubleVyAA4IsP2VAA0F2P3VGACyxq_GAA2P1A2A0G0RzAA0H0R_rlWl()
// CHECK-NEXT:    call swiftcc void @_T042conditional_conformance_basic_conformances8takes_p1yxmAA2P1RzlF(%swift.type* [[Double_TYPE]], %swift.type* [[Double_TYPE]], i8** [[Double_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// Lazy witness table accessor for the concrete Double<IsP2, IsP3> : P1.

// CHECK-LABEL: define linkonce_odr hidden i8** @_T042conditional_conformance_basic_conformances6DoubleVyAA4IsP2VAA0F2P3VGACyxq_GAA2P1A2A0G0RzAA0H0R_rlWl() #1 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [2 x i8**], align 8
// CHECK-NEXT:    [[CACHE:%.*]] = load i8**, i8*** @_T042conditional_conformance_basic_conformances6DoubleVyAA4IsP2VAA0F2P3VGACyxq_GAA2P1A2A0G0RzAA0H0R_rlWL, align 8
// CHECK-NEXT:    [[IS_NULL:%.*]] = icmp eq i8** [[CACHE]], null
// CHECK-NEXT:    br i1 [[IS_NULL]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:    [[Double_TYPE:%.*]] = call %swift.type* @_T042conditional_conformance_basic_conformances6DoubleVyAA4IsP2VAA0F2P3VGMa()

// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [2 x i8**], [2 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[B_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** getelementptr inbounds ([0 x i8*], [0 x i8*]* @_T042conditional_conformance_basic_conformances4IsP2VAA0F0AAWP, i32 0, i32 0), i8*** [[B_P2_PTR]], align 8
// CHECK-NEXT:    [[C_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 1
// CHECK-NEXT:    store i8** getelementptr inbounds ([0 x i8*], [0 x i8*]* @_T042conditional_conformance_basic_conformances4IsP3VAA0F0AAWP, i32 0, i32 0), i8*** [[C_P3_PTR]], align 8

// CHECK-NEXT:    [[Double_P1:%.*]] = call i8** @_T042conditional_conformance_basic_conformances6DoubleVyxq_GAA2P1A2A2P2RzAA2P3R_rlWa(%swift.type* [[Double_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]], i64 2)
// CHECK-NEXT:    store atomic i8** [[Double_P1]], i8*** @_T042conditional_conformance_basic_conformances6DoubleVyAA4IsP2VAA0F2P3VGACyxq_GAA2P1A2A0G0RzAA0H0R_rlWL release, align 8
// CHECK-NEXT:    br label %cont

// CHECK:       cont:
// CHECK-NEXT:    %7 = phi i8** [ [[CACHE]], %entry ], [ [[Double_P1]], %cacheIsNull ]
// CHECK-NEXT:    ret i8** %7
// CHECK-NEXT:  }


// # Witness table instantiators

// witness table instantiator for Single : P1

// CHECK-LABEL: define internal void @_T042conditional_conformance_basic_conformances6SingleVyxGAA2P1A2A2P2RzlWI(i8**, %swift.type*, i8**) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[CONDITIONAL_TABLE_SLICE:%.*]] = bitcast i8** %2 to %swift.witness_table_slice*
// CHECK-NEXT:    [[TABLES_PTR:%.*]] = getelementptr inbounds %swift.witness_table_slice, %swift.witness_table_slice* [[CONDITIONAL_TABLE_SLICE]], i32 0, i32 0
// CHECK-NEXT:    [[TABLES:%.*]] = load i8***, i8**** [[TABLES_PTR]], align 8
// CHECK-NEXT:    [[COUNT_PTR:%.*]] = getelementptr inbounds %swift.witness_table_slice, %swift.witness_table_slice* [[CONDITIONAL_TABLE_SLICE]], i32 0, i32 1
// CHECK-NEXT:    [[COUNT:%.*]] = load i64, i64* [[COUNT_PTR]], align 8
// CHECK-NEXT:    [[COND:%.*]] = icmp eq i64 [[COUNT]], 1
// CHECK-NEXT:    br i1 [[COND]], label %cont, label %bad_witness_table_count

// CHECK:       cont:
// CHECK-NEXT:    [[A_P2_SRC:%.*]] = getelementptr inbounds i8**, i8*** [[TABLES]], i32 0
// CHECK-NEXT:    [[A_P2_DEST:%.*]] = getelementptr inbounds i8*, i8** %0, i32 -1
// CHECK-NEXT:    [[A_P2:%.*]] = load i8**, i8*** [[A_P2_SRC]], align 8
// CHECK-NEXT:    [[CAST_A_P2_DEST:%.*]] = bitcast i8** [[A_P2_DEST]] to i8***
// CHECK-NEXT:    store i8** [[A_P2]], i8*** [[CAST_A_P2_DEST]], align 8

// CHECK-NEXT:    ret void

// CHECK:       bad_witness_table_count:
// CHECK-NEXT:    call void @llvm.trap()
// CHECK-NEXT:    unreachable
// CHECK-NEXT:  }

// witness table instantiator for Double : P1

// CHECK-LABEL: define internal void @_T042conditional_conformance_basic_conformances6DoubleVyxq_GAA2P1A2A2P2RzAA2P3R_rlWI(i8**, %swift.type*, i8**) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[CONDITIONAL_TABLE_SLICE:%.*]] = bitcast i8** %2 to %swift.witness_table_slice*
// CHECK-NEXT:    [[TABLES_PTR:%.*]] = getelementptr inbounds %swift.witness_table_slice, %swift.witness_table_slice* [[CONDITIONAL_TABLE_SLICE]], i32 0, i32 0
// CHECK-NEXT:    [[TABLES:%.*]] = load i8***, i8**** [[TABLES_PTR]], align 8
// CHECK-NEXT:    [[COUNT_PTR:%.*]] = getelementptr inbounds %swift.witness_table_slice, %swift.witness_table_slice* [[CONDITIONAL_TABLE_SLICE]], i32 0, i32 1
// CHECK-NEXT:    [[COUNT:%.*]] = load i64, i64* [[COUNT_PTR]], align 8
// CHECK-NEXT:    [[COND:%.*]] = icmp eq i64 [[COUNT]], 2
// CHECK-NEXT:    br i1 [[COND]], label %cont, label %bad_witness_table_count

// CHECK:       cont:
// CHECK-NEXT:    [[B_P2_SRC:%.*]] = getelementptr inbounds i8**, i8*** [[TABLES]], i32 0
// CHECK-NEXT:    [[B_P2_DEST:%.*]] = getelementptr inbounds i8*, i8** %0, i32 -1
// CHECK-NEXT:    [[B_P2:%.*]] = load i8**, i8*** [[B_P2_SRC]], align 8
// CHECK-NEXT:    [[CAST_B_P2_DEST:%.*]] = bitcast i8** [[B_P2_DEST]] to i8***
// CHECK-NEXT:    store i8** [[B_P2]], i8*** [[CAST_B_P2_DEST]], align 8

// CHECK-NEXT:    [[C_P3_SRC:%.*]] = getelementptr inbounds i8**, i8*** [[TABLES]], i32 1
// CHECK-NEXT:    [[C_P3_DEST:%.*]] = getelementptr inbounds i8*, i8** %0, i32 -2
// CHECK-NEXT:    [[C_P3:%.*]] = load i8**, i8*** [[C_P3_SRC]], align 8
// CHECK-NEXT:    [[CAST_C_P3_DEST:%.*]] = bitcast i8** [[C_P3_DEST]] to i8***
// CHECK-NEXT:    store i8** [[C_P3]], i8*** [[CAST_C_P3_DEST]], align 8

// CHECK-NEXT:    ret void

// CHECK:       bad_witness_table_count:
// CHECK-NEXT:    call void @llvm.trap()
// CHECK-NEXT:    unreachable
// CHECK-NEXT:  }

func dynamicCastToP1(_ value: Any) -> P1? {
  return value as? P1
}

// CHECK: attributes [[ATTRS]] = { nounwind }
