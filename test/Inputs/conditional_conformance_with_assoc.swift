public func takes_p1<T: P1>(_: T.Type) {}
public protocol P1 {
  associatedtype AT1

  func normal()
  func generic<T: P3>(_: T)
}
public protocol P2 {
  associatedtype AT2: P3
}
public protocol P3 {
  associatedtype AT3
}

public struct Nothing {}

public struct IsP2: P2 {
  public typealias AT2 = IsP3
}
public struct IsP3: P3 {
  public typealias AT3 = Nothing
}

public struct IsAlsoP2: P2 {
  public typealias AT2 = IsBoth
}
public struct IsBoth: P2, P3 {
  public typealias AT2 = HoldsP3
  public typealias AT3 = Nothing
}
public struct HoldsP3: P3 {
  public typealias AT3 = IsP3
}

public struct Double<B: P2, C> {}
extension Double: P1 where B.AT2: P2, C: P3, B.AT2.AT2.AT3: P3 {
  public typealias AT1 = C
  public func normal() {}
  public func generic<T: P3>(_: T) {}
}

// witness method for Double.normal

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @_T034conditional_conformance_with_assoc6DoubleVyxq_GAA2P1A2A2P3R_AA2P23AT2RpzAafH_AhaGP3AT3RPzrlAaEP6normalyyFTW(%T34conditional_conformance_with_assoc6DoubleV* noalias nocapture swiftself, %swift.type* %Self, i8** %SelfWitnessTable) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[C_P3_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -1
// CHECK-NEXT:    [[C_P3:%.*]] = load i8*, i8** [[C_P3_PTR]], align 8
// CHECK-NEXT:    %"\CF\84_0_1.P3" = bitcast i8* [[C_P3]] to i8**

// CHECK-NEXT:    [[B_AT2_P2_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -2
// CHECK-NEXT:    [[B_AT2_P2:%.*]] = load i8*, i8** [[B_AT2_P2_PTR]], align 8
// CHECK-NEXT:    %"\CF\84_0_0.AT2.P2" = bitcast i8* [[B_AT2_P2]] to i8**

// CHECK-NEXT:    [[B_AT2_AT2_AT3_P3_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -3
// CHECK-NEXT:    [[B_AT2_AT2_AT3_P3:%.*]] = load i8*, i8** [[B_AT2_AT2_AT3_P3_PTR]], align 8
// CHECK-NEXT:    %"\CF\84_0_0.AT2.AT2.AT3.P3" = bitcast i8* [[B_AT2_AT2_AT3_P3]] to i8**

// CHECK-NEXT:    [[SELF_AS_TYPE_ARRAY:%.*]] = bitcast %swift.type* %Self to %swift.type**
// CHECK-NEXT:    [[B_PTR:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[SELF_AS_TYPE_ARRAY]], i64 2
// CHECK-NEXT:    [[B:%.*]] = load %swift.type*, %swift.type** [[B_PTR]], align 8

// CHECK-NEXT:    [[SELF_AS_TYPE_ARRAY_2:%.*]] = bitcast %swift.type* %Self to %swift.type**
// CHECK-NEXT:    [[C_PTR:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[SELF_AS_TYPE_ARRAY_2]], i64 3
// CHECK-NEXT:    [[C:%.*]] = load %swift.type*, %swift.type** [[C_PTR]], align 8

// CHECK-NEXT:    [[SELF_AS_WT_ARRAY:%.*]] = bitcast %swift.type* %Self to i8***
// CHECK-NEXT:    [[B_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[SELF_AS_WT_ARRAY]], i64 4
// CHECK-NEXT:    %"\CF\84_0_0.P2" = load i8**, i8*** [[B_P2_PTR]], align 8

// CHECK-NEXT:    call swiftcc void @_T034conditional_conformance_with_assoc6DoubleVA2A2P3R_AA2P23AT2RpzAadF_AfaEP3AT3RPzrlE6normalyyF(%swift.type* %"\CF\84_0_0", %swift.type* %"\CF\84_0_1", i8** %"\CF\84_0_0.P2", i8** %"\CF\84_0_1.P3", i8** %"\CF\84_0_0.AT2.P2", i8** %"\CF\84_0_0.AT2.AT2.AT3.P3")
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// witness method for Double.generic

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @_T034conditional_conformance_with_assoc6DoubleVyxq_GAA2P1A2A2P3R_AA2P23AT2RpzAafH_AhaGP3AT3RPzrlAaEP7genericyqd__AaFRd__lFTW(%swift.opaque* noalias nocapture, %swift.type* %"\CF\84_1_0", i8** %"\CF\84_1_0.P3", %T34conditional_conformance_with_assoc6DoubleV* noalias nocapture swiftself, %swift.type* %Self, i8** %SelfWitnessTable) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[C_P3_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -1
// CHECK-NEXT:    [[C_P3:%.*]] = load i8*, i8** [[C_P3_PTR]], align 8
// CHECK-NEXT:    %"\CF\84_0_1.P3" = bitcast i8* [[C_P3]] to i8**

// CHECK-NEXT:    [[B_AT2_P2_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -2
// CHECK-NEXT:    [[B_AT2_P2:%.*]] = load i8*, i8** [[B_AT2_P2_PTR]], align 8
// CHECK-NEXT:    %"\CF\84_0_0.AT2.P2" = bitcast i8* [[B_AT2_P2]] to i8**

// CHECK-NEXT:    [[B_AT2_AT2_AT3_P3_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -3
// CHECK-NEXT:    [[B_AT2_AT2_AT3_P3:%.*]] = load i8*, i8** [[B_AT2_AT2_AT3_P3_PTR]], align 8
// CHECK-NEXT:    %"\CF\84_0_0.AT2.AT2.AT3.P3" = bitcast i8* [[B_AT2_AT2_AT3_P3]] to i8**

// CHECK-NEXT:    [[SELF_AS_TYPE_ARRAY:%.*]] = bitcast %swift.type* %Self to %swift.type**
// CHECK-NEXT:    [[B_PTR:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[SELF_AS_TYPE_ARRAY]], i64 2
// CHECK-NEXT:    [[B:%.*]] = load %swift.type*, %swift.type** [[B_PTR]], align 8

// CHECK-NEXT:    [[SELF_AS_TYPE_ARRAY_2:%.*]] = bitcast %swift.type* %Self to %swift.type**
// CHECK-NEXT:    [[C_PTR:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[SELF_AS_TYPE_ARRAY_2]], i64 3
// CHECK-NEXT:    [[C:%.*]] = load %swift.type*, %swift.type** [[C_PTR]], align 8

// CHECK-NEXT:    [[SELF_AS_WT_ARRAY:%.*]] = bitcast %swift.type* %Self to i8***
// CHECK-NEXT:    [[B_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[SELF_AS_WT_ARRAY]], i64 4
// CHECK-NEXT:    %"\CF\84_0_0.P2" = load i8**, i8*** [[B_P2_PTR]], align 8

// CHECK-NEXT:    call swiftcc void @_T034conditional_conformance_with_assoc6DoubleVA2A2P3R_AA2P23AT2RpzAadF_AfaEP3AT3RPzrlE7genericyqd__AaDRd__lF(%swift.opaque* noalias nocapture %0, %swift.type* %"\CF\84_0_0", %swift.type* %"\CF\84_0_1", %swift.type* %"\CF\84_1_0", i8** %"\CF\84_0_0.P2", i8** %"\CF\84_0_1.P3", i8** %"\CF\84_1_0.P3", i8** %"\CF\84_0_0.AT2.P2", i8** %"\CF\84_0_0.AT2.AT2.AT3.P3")
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }


public func generic_generic<T: P2, U>(_: T.Type, _: U.Type)
  where T.AT2: P2, U: P3, T.AT2.AT2.AT3: P3
{
  takes_p1(Double<T, U>.self)
}
// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T034conditional_conformance_with_assoc08generic_E0yxm_q_mtAA2P2RzAA2P3R_AaC3AT2RpzAadE_AeaCP3AT3RPzr0_lF(%swift.type*, %swift.type*, %swift.type* %T, %swift.type* %U, i8** %T.P2, i8** %U.P3, i8** %T.AT2.P2, i8** %T.AT2.AT2.AT3.P3) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [3 x i8**], align 8
// CHECK-NEXT:    [[Double_TYPE:%.*]] = call %swift.type* @_T034conditional_conformance_with_assoc6DoubleVMa(%swift.type* %T, %swift.type* %U, i8** %T.P2)

// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [3 x i8**], [3 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[C_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** %U.P3, i8*** [[C_P3_PTR]], align 8
// CHECK-NEXT:    [[B_AT2_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 1
// CHECK-NEXT:    store i8** %T.AT2.P2, i8*** [[B_AT2_P2_PTR]], align 8
// CHECK-NEXT:    [[B_AT2_AT2_AT3_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 2
// CHECK-NEXT:    store i8** %T.AT2.AT2.AT3.P3, i8*** [[B_AT2_AT2_AT3_P3_PTR]], align 8

// CHECK-NEXT:    [[Double_P1:%.*]] = call i8** @_T034conditional_conformance_with_assoc6DoubleVyxq_GAA2P1A2A2P3R_AA2P23AT2RpzAafH_AhaGP3AT3RPzrlWa(%swift.type* [[Double_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]], i64 3)
// CHECK-NEXT:    call swiftcc void @_T034conditional_conformance_with_assoc8takes_p1yxmAA2P1RzlF(%swift.type* [[Double_TYPE]], %swift.type* [[Double_TYPE]], i8** [[Double_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

public func generic_concrete<T: P2>(_: T.Type)
  where T.AT2: P2, T.AT2.AT2.AT3: P3
{
  takes_p1(Double<T, IsP3>.self)
}
// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T034conditional_conformance_with_assoc16generic_concreteyxmAA2P2RzAaC3AT2RpzAA2P3AD_AdaCP3AT3RPzlF(%swift.type*, %swift.type* %T, i8** %T.P2, i8** %T.AT2.P2, i8** %T.AT2.AT2.AT3.P3) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [3 x i8**], align 8
// CHECK-NEXT:    [[Double_TYPE:%.*]] = call %swift.type* @_T034conditional_conformance_with_assoc6DoubleVMa(%swift.type* %T, %swift.type* bitcast (i64* getelementptr inbounds (<{ i8**, i64, <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i32 }>* }>, <{ i8**, i64, <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i32 }>* }>* @_T034conditional_conformance_with_assoc4IsP3VMf, i32 0, i32 1) to %swift.type*), i8** %T.P2)

// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [3 x i8**], [3 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[C_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @_T034conditional_conformance_with_assoc4IsP3VAA0F0AAWP, i32 0, i32 0), i8*** [[C_P3_PTR]], align 8
// CHECK-NEXT:    [[B_AT2_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 1
// CHECK-NEXT:    store i8** %T.AT2.P2, i8*** [[B_AT2_P2_PTR]], align 8
// CHECK-NEXT:    [[B_AT2_AT2_AT3_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 2
// CHECK-NEXT:    store i8** %T.AT2.AT2.AT3.P3, i8*** [[B_AT2_AT2_AT3_P3_PTR]], align 8

// CHECK-NEXT:    [[Double_P1:%.*]] = call i8** @_T034conditional_conformance_with_assoc6DoubleVyxq_GAA2P1A2A2P3R_AA2P23AT2RpzAafH_AhaGP3AT3RPzrlWa(%swift.type* [[Double_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]], i64 3)
// CHECK-NEXT:    call swiftcc void @_T034conditional_conformance_with_assoc8takes_p1yxmAA2P1RzlF(%swift.type* [[Double_TYPE]], %swift.type* [[Double_TYPE]], i8** [[Double_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }


public func concrete_generic<U>(_: U.Type)
  where U: P3
{
  takes_p1(Double<IsAlsoP2, U>.self)
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T034conditional_conformance_with_assoc16concrete_genericyxmAA2P3RzlF(%swift.type*, %swift.type* %U, i8** %U.P3) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:  %conditional.requirement.buffer = alloca [3 x i8**], align 8
// CHECK-NEXT:  [[Double_TYPE:%.*]] = call %swift.type* @_T034conditional_conformance_with_assoc6DoubleVMa(%swift.type* bitcast (i64* getelementptr inbounds (<{ i8**, i64, <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i32 }>* }>, <{ i8**, i64, <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i32 }>* }>* @_T034conditional_conformance_with_assoc8IsAlsoP2VMf, i32 0, i32 1) to %swift.type*), %swift.type* %U, i8** getelementptr inbounds ([2 x i8*], [2 x i8*]* @_T034conditional_conformance_with_assoc8IsAlsoP2VAA0G0AAWP, i32 0, i32 0))
// CHECK-NEXT:  [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [3 x i8**], [3 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:  [[C_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:  store i8** %U.P3, i8*** [[C_P3_PTR]], align 8
// CHECK-NEXT:  [[B_AT2_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 1
// CHECK-NEXT:  store i8** getelementptr inbounds ([2 x i8*], [2 x i8*]* @_T034conditional_conformance_with_assoc6IsBothVAA2P2AAWP, i32 0, i32 0), i8*** [[B_AT2_P2_PTR]], align 8
// CHECK-NEXT:  [[B_AT2_AT2_AT3_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 2
// CHECK-NEXT:  store i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @_T034conditional_conformance_with_assoc4IsP3VAA0F0AAWP, i32 0, i32 0), i8*** [[B_AT2_AT2_AT3_P3_PTR]], align 8
// CHECK-NEXT:  [[Double_P1:%.*]] = call i8** @_T034conditional_conformance_with_assoc6DoubleVyxq_GAA2P1A2A2P3R_AA2P23AT2RpzAafH_AhaGP3AT3RPzrlWa(%swift.type* [[Double_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]], i64 3)
// CHECK-NEXT:  call swiftcc void @_T034conditional_conformance_with_assoc8takes_p1yxmAA2P1RzlF(%swift.type* [[Double_TYPE]], %swift.type* [[Double_TYPE]], i8** [[Double_P1]])
// CHECK-NEXT:  ret void
// CHECK-NEXT:}


public func concrete_concrete() {
  takes_p1(Double<IsAlsoP2, IsP3>.self)
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T034conditional_conformance_with_assoc09concrete_E0yyF() #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %0 = call %swift.type* @_T034conditional_conformance_with_assoc6DoubleVyAA8IsAlsoP2VAA0F2P3VGMa() #10
// CHECK-NEXT:    %1 = call i8** @_T034conditional_conformance_with_assoc6DoubleVyAA8IsAlsoP2VAA0F2P3VGACyxq_GAA2P1A2A0I0R_AA0H03AT2RpzAakM_AmaLP3AT3RPzrlWl() #10
// CHECK-NEXT:    call swiftcc void @_T034conditional_conformance_with_assoc8takes_p1yxmAA2P1RzlF(%swift.type* %0, %swift.type* %0, i8** %1)
// CHECK-NEXT:    ret void
// CHECK-NEXT: }

// Lazy witness table accessor for the concrete Double<IsAlsoP2, IsP3> : P1.

// CHECK-LABEL: define linkonce_odr hidden i8** @_T034conditional_conformance_with_assoc6DoubleVyAA8IsAlsoP2VAA0F2P3VGACyxq_GAA2P1A2A0I0R_AA0H03AT2RpzAakM_AmaLP3AT3RPzrlWl() #1 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [3 x i8**], align 8
// CHECK-NEXT:    [[CACHE:%.*]] = load i8**, i8*** @_T034conditional_conformance_with_assoc6DoubleVyAA8IsAlsoP2VAA0F2P3VGACyxq_GAA2P1A2A0I0R_AA0H03AT2RpzAakM_AmaLP3AT3RPzrlWL, align 8
// CHECK-NEXT:    [[IS_NULL:%.*]] = icmp eq i8** [[CACHE]], null
// CHECK-NEXT:    br i1 [[IS_NULL]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:    [[Double_TYPE:%.*]] = call %swift.type* @_T034conditional_conformance_with_assoc6DoubleVyAA8IsAlsoP2VAA0F2P3VGMa() #10
// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [3 x i8**], [3 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[C_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @_T034conditional_conformance_with_assoc4IsP3VAA0F0AAWP, i32 0, i32 0), i8*** [[C_P3_PTR]], align 8
// CHECK-NEXT:    [[B_AT2_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 1
// CHECK-NEXT:    store i8** getelementptr inbounds ([2 x i8*], [2 x i8*]* @_T034conditional_conformance_with_assoc6IsBothVAA2P2AAWP, i32 0, i32 0), i8*** [[B_AT2_P2_PTR]], align 8
// CHECK-NEXT:    [[B_AT2_AT2_AT3_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 2
// CHECK-NEXT:    store i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @_T034conditional_conformance_with_assoc4IsP3VAA0F0AAWP, i32 0, i32 0), i8*** [[B_AT2_AT2_AT3_P3_PTR]], align 8
// CHECK-NEXT:    [[Double_P1:%.*]] = call i8** @_T034conditional_conformance_with_assoc6DoubleVyxq_GAA2P1A2A2P3R_AA2P23AT2RpzAafH_AhaGP3AT3RPzrlWa(%swift.type* [[Double_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]], i64 3)
// CHECK-NEXT:    store atomic i8** [[Double_P1]], i8*** @_T034conditional_conformance_with_assoc6DoubleVyAA8IsAlsoP2VAA0F2P3VGACyxq_GAA2P1A2A0I0R_AA0H03AT2RpzAakM_AmaLP3AT3RPzrlWL release, align 8
// CHECK-NEXT:    br label %cont

// CHECK:       cont:
// CHECK-NEXT:    %8 = phi i8** [ [[CACHE]], %entry ], [ [[Double_P1]], %cacheIsNull ]
// CHECK-NEXT:    ret i8** %8
// CHECK-NEXT:  }



// witness table instantiator for Double : P1

// CHECK-LABEL: define internal void @_T034conditional_conformance_with_assoc6DoubleVyxq_GAA2P1A2A2P3R_AA2P23AT2RpzAafH_AhaGP3AT3RPzrlWI(i8**, %swift.type*, i8**) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[CONDITIONAL_TABLE_SLICE:%.*]] = bitcast i8** %2 to %swift.witness_table_slice*
// CHECK-NEXT:    [[TABLES_PTR:%.*]] = getelementptr inbounds %swift.witness_table_slice, %swift.witness_table_slice* [[CONDITIONAL_TABLE_SLICE]], i32 0, i32 0
// CHECK-NEXT:    [[TABLES:%.*]] = load i8***, i8**** [[TABLES_PTR]], align 8
// CHECK-NEXT:    [[COUNT_PTR:%.*]] = getelementptr inbounds %swift.witness_table_slice, %swift.witness_table_slice* [[CONDITIONAL_TABLE_SLICE]], i32 0, i32 1
// CHECK-NEXT:    [[COUNT:%.*]] = load i64, i64* [[COUNT_PTR]], align 8
// CHECK-NEXT:    [[COND:%.*]] = icmp eq i64 [[COUNT]],
// CHECK-NEXT:    br i1 [[COND]], label %cont, label %bad_witness_table_count

// CHECK:       cont:
// CHECK-NEXT:    [[C_P3_SRC:%.*]] = getelementptr inbounds i8**, i8*** [[TABLES]], i32 0
// CHECK-NEXT:    [[C_P3_DEST:%.*]] = getelementptr inbounds i8*, i8** %0, i32 -1
// CHECK-NEXT:    [[C_P3:%.*]] = load i8**, i8*** [[C_P3_SRC]], align 8
// CHECK-NEXT:    [[CAST_C_P3_DEST:%.*]] = bitcast i8** [[C_P3_DEST]] to i8***
// CHECK-NEXT:    store i8** [[C_P3]], i8*** [[CAST_C_P3_DEST]], align 8

// CHECK-NEXT:    [[B_AT2_P2_SRC:%.*]] = getelementptr inbounds i8**, i8*** [[TABLES]], i32 1
// CHECK-NEXT:    [[B_AT2_P2_DEST:%.*]] = getelementptr inbounds i8*, i8** %0, i32 -2
// CHECK-NEXT:    [[B_AT2_P2:%.*]] = load i8**, i8*** [[B_AT2_P2_SRC]], align 8
// CHECK-NEXT:    [[CAST_B_AT2_P2_DEST:%.*]] = bitcast i8** [[B_AT2_P2_DEST]] to i8***
// CHECK-NEXT:    store i8** [[B_AT2_P2]], i8*** [[CAST_B_AT2_P2_DEST]], align 8

// CHECK-NEXT:    [[B_AT2_AT2_AT3_P3_SRC:%.*]] = getelementptr inbounds i8**, i8*** [[TABLES]], i32 2
// CHECK-NEXT:    [[B_AT2_AT2_AT3_P3_DEST:%.*]] = getelementptr inbounds i8*, i8** %0, i32 -3
// CHECK-NEXT:    [[B_AT2_AT2_AT3_P3:%.*]] = load i8**, i8*** [[B_AT2_AT2_AT3_P3_SRC]], align 8
// CHECK-NEXT:    [[CAST_B_AT2_AT2_AT3_P3_DEST:%.*]] = bitcast i8** [[B_AT2_AT2_AT3_P3_DEST]] to i8***
// CHECK-NEXT:    store i8** [[B_AT2_AT2_AT3_P3]], i8*** [[CAST_B_AT2_AT2_AT3_P3_DEST]], align 8

// CHECK-NEXT:    ret void

// CHECK:       bad_witness_table_count:
// CHECK-NEXT:    call void @llvm.trap()
// CHECK-NEXT:    unreachable
// CHECK-NEXT:  }

