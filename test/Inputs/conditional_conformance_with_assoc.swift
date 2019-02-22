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

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @"$s34conditional_conformance_with_assoc6DoubleVyxq_GAA2P1A2A2P3R_AA2P23AT2RpzAafH_AhaGP3AT3RPzrlAaEP6normalyyFTW"(%T34conditional_conformance_with_assoc6DoubleV* noalias nocapture swiftself, %swift.type* %Self, i8** %SelfWitnessTable)
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

// CHECK-NEXT:    call swiftcc void @"$s34conditional_conformance_with_assoc6DoubleVA2A2P3R_AA2P23AT2RpzAadF_AfaEP3AT3RPzrlE6normalyyF"(%swift.type* %"\CF\84_0_0", %swift.type* %"\CF\84_0_1", i8** %"\CF\84_0_0.P2", i8** %"\CF\84_0_1.P3", i8** %"\CF\84_0_0.AT2.P2", i8** %"\CF\84_0_0.AT2.AT2.AT3.P3")
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// witness method for Double.generic

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @"$s34conditional_conformance_with_assoc6DoubleVyxq_GAA2P1A2A2P3R_AA2P23AT2RpzAafH_AhaGP3AT3RPzrlAaEP7genericyyqd__AaFRd__lFTW"(%swift.opaque* noalias nocapture, %swift.type* %"\CF\84_1_0", i8** %"\CF\84_1_0.P3", %T34conditional_conformance_with_assoc6DoubleV* noalias nocapture swiftself, %swift.type* %Self, i8** %SelfWitnessTable)
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

// CHECK-NEXT:    call swiftcc void @"$s34conditional_conformance_with_assoc6DoubleVA2A2P3R_AA2P23AT2RpzAadF_AfaEP3AT3RPzrlE7genericyyqd__AaDRd__lF"(%swift.opaque* noalias nocapture %0, %swift.type* %"\CF\84_0_0", %swift.type* %"\CF\84_0_1", %swift.type* %"\CF\84_1_0", i8** %"\CF\84_0_0.P2", i8** %"\CF\84_0_1.P3", i8** %"\CF\84_1_0.P3", i8** %"\CF\84_0_0.AT2.P2", i8** %"\CF\84_0_0.AT2.AT2.AT3.P3")
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }


public func generic_generic<T: P2, U>(_: T.Type, _: U.Type)
  where T.AT2: P2, U: P3, T.AT2.AT2.AT3: P3
{
  takes_p1(Double<T, U>.self)
}
// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s34conditional_conformance_with_assoc08generic_E0yyxm_q_mtAA2P2RzAA2P3R_AaC3AT2RpzAadE_AeaCP3AT3RPzr0_lF"(%swift.type*, %swift.type*, %swift.type* %T, %swift.type* %U, i8** %T.P2, i8** %U.P3, i8** %T.AT2.P2, i8** %T.AT2.AT2.AT3.P3)
// CHECK-NEXT:  entry:
// CHECK:         %conditional.requirement.buffer = alloca [3 x i8**], align 8
// CHECK:         [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s34conditional_conformance_with_assoc6DoubleVMa"(i64 0, %swift.type* %T, %swift.type* %U, i8** %T.P2)
// CHECK-NEXT:    [[Double_TYPE:%.*]] = extractvalue %swift.metadata_response [[T0]], 0

// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [3 x i8**], [3 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[C_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** %U.P3, i8*** [[C_P3_PTR]], align 8
// CHECK-NEXT:    [[B_AT2_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 1
// CHECK-NEXT:    store i8** %T.AT2.P2, i8*** [[B_AT2_P2_PTR]], align 8
// CHECK-NEXT:    [[B_AT2_AT2_AT3_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 2
// CHECK-NEXT:    store i8** %T.AT2.AT2.AT3.P3, i8*** [[B_AT2_AT2_AT3_P3_PTR]], align 8

// CHECK-NEXT:    [[Double_P1:%.*]] = call i8** @swift_getWitnessTable
// CHECK-NEXT:    call swiftcc void @"$s34conditional_conformance_with_assoc8takes_p1yyxmAA2P1RzlF"(%swift.type* [[Double_TYPE]], %swift.type* [[Double_TYPE]], i8** [[Double_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

public func generic_concrete<T: P2>(_: T.Type)
  where T.AT2: P2, T.AT2.AT2.AT3: P3
{
  takes_p1(Double<T, IsP3>.self)
}
// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s34conditional_conformance_with_assoc16generic_concreteyyxmAA2P2RzAaC3AT2RpzAA2P3AD_AdaCP3AT3RPzlF"(%swift.type*, %swift.type* %T, i8** %T.P2, i8** %T.AT2.P2, i8** %T.AT2.AT2.AT3.P3)
// CHECK-NEXT:  entry:
// CHECK:         %conditional.requirement.buffer = alloca [3 x i8**], align 8
// CHECK:         [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s34conditional_conformance_with_assoc6DoubleVMa"(i64 0, %swift.type* %T, %swift.type* bitcast (i64* getelementptr inbounds (<{ i8**, i64, <{ {{.*}} }>* }>, <{ {{.*}} }>* @"$s34conditional_conformance_with_assoc4IsP3VMf", i32 0, i32 1) to %swift.type*), i8** %T.P2)
// CHECK-NEXT:    [[Double_TYPE:%.*]] = extractvalue %swift.metadata_response [[T0]], 0

// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [3 x i8**], [3 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[C_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** getelementptr inbounds ([2 x i8*], [2 x i8*]* @"$s34conditional_conformance_with_assoc4IsP3VAA0F0AAWP", i32 0, i32 0), i8*** [[C_P3_PTR]], align 8
// CHECK-NEXT:    [[B_AT2_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 1
// CHECK-NEXT:    store i8** %T.AT2.P2, i8*** [[B_AT2_P2_PTR]], align 8
// CHECK-NEXT:    [[B_AT2_AT2_AT3_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 2
// CHECK-NEXT:    store i8** %T.AT2.AT2.AT3.P3, i8*** [[B_AT2_AT2_AT3_P3_PTR]], align 8

// CHECK-NEXT:    [[Double_P1:%.*]] = call i8** @swift_getWitnessTable
// CHECK-NEXT:    call swiftcc void @"$s34conditional_conformance_with_assoc8takes_p1yyxmAA2P1RzlF"(%swift.type* [[Double_TYPE]], %swift.type* [[Double_TYPE]], i8** [[Double_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }


public func concrete_generic<U>(_: U.Type)
  where U: P3
{
  takes_p1(Double<IsAlsoP2, U>.self)
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s34conditional_conformance_with_assoc16concrete_genericyyxmAA2P3RzlF"(%swift.type*, %swift.type* %U, i8** %U.P3)
// CHECK-NEXT:  entry:
// CHECK:       %conditional.requirement.buffer = alloca [3 x i8**], align 8
// CHECK:       [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s34conditional_conformance_with_assoc6DoubleVMa"(i64 0, %swift.type* bitcast (i64* getelementptr inbounds (<{ {{.*}} }>, <{ {{.*}} }>* @"$s34conditional_conformance_with_assoc8IsAlsoP2VMf", i32 0, i32 1) to %swift.type*), %swift.type* %U, i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @"$s34conditional_conformance_with_assoc8IsAlsoP2VAA0G0AAWP", i32 0, i32 0))
// CHECK-NEXT:  [[Double_TYPE:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT:  [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [3 x i8**], [3 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:  [[C_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:  store i8** %U.P3, i8*** [[C_P3_PTR]], align 8
// CHECK-NEXT:  [[B_AT2_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 1
// CHECK-NEXT:  store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @"$s34conditional_conformance_with_assoc6IsBothVAA2P2AAWP", i32 0, i32 0), i8*** [[B_AT2_P2_PTR]], align 8
// CHECK-NEXT:  [[B_AT2_AT2_AT3_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 2
// CHECK-NEXT:  store i8** getelementptr inbounds ([2 x i8*], [2 x i8*]* @"$s34conditional_conformance_with_assoc4IsP3VAA0F0AAWP", i32 0, i32 0), i8*** [[B_AT2_AT2_AT3_P3_PTR]], align 8
// CHECK-NEXT:  [[Double_P1:%.*]] = call i8** @swift_getWitnessTable
// CHECK-NEXT:  call swiftcc void @"$s34conditional_conformance_with_assoc8takes_p1yyxmAA2P1RzlF"(%swift.type* [[Double_TYPE]], %swift.type* [[Double_TYPE]], i8** [[Double_P1]])
// CHECK-NEXT:  ret void
// CHECK-NEXT:}


public func concrete_concrete() {
  takes_p1(Double<IsAlsoP2, IsP3>.self)
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s34conditional_conformance_with_assoc09concrete_E0yyF"()
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s34conditional_conformance_with_assoc6DoubleVyAA8IsAlsoP2VAA0F2P3VGMa"(i64 0)
// CHECK-NEXT:    [[X:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT:    [[Z:%.*]] = call i8** @"$s34conditional_conformance_with_assoc6DoubleVyAA8IsAlsoP2VAA0F2P3VGACyxq_GAA2P1A2A0I0R_AA0H03AT2RpzAakM_AmaLP3AT3RPzrlWl"()
// CHECK-NEXT:    call swiftcc void @"$s34conditional_conformance_with_assoc8takes_p1yyxmAA2P1RzlF"(%swift.type* [[X]], %swift.type* [[X]], i8** [[Z]])
// CHECK-NEXT:    ret void
// CHECK-NEXT: }

// Lazy witness table accessor for the concrete Double<IsAlsoP2, IsP3> : P1.

// CHECK-LABEL: define linkonce_odr hidden i8** @"$s34conditional_conformance_with_assoc6DoubleVyAA8IsAlsoP2VAA0F2P3VGACyxq_GAA2P1A2A0I0R_AA0H03AT2RpzAakM_AmaLP3AT3RPzrlWl"()
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [3 x i8**], align 8
// CHECK-NEXT:    [[CACHE:%.*]] = load i8**, i8*** @"$s34conditional_conformance_with_assoc6DoubleVyAA8IsAlsoP2VAA0F2P3VGACyxq_GAA2P1A2A0I0R_AA0H03AT2RpzAakM_AmaLP3AT3RPzrlWL", align 8
// CHECK-NEXT:    [[IS_NULL:%.*]] = icmp eq i8** [[CACHE]], null
// CHECK-NEXT:    br i1 [[IS_NULL]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:    [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s34conditional_conformance_with_assoc6DoubleVyAA8IsAlsoP2VAA0F2P3VGMa"(i64 0)
// CHECK-NEXT:    [[Double_TYPE:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [3 x i8**], [3 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[C_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** getelementptr inbounds ([2 x i8*], [2 x i8*]* @"$s34conditional_conformance_with_assoc4IsP3VAA0F0AAWP", i32 0, i32 0), i8*** [[C_P3_PTR]], align 8
// CHECK-NEXT:    [[B_AT2_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 1
// CHECK-NEXT:    store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @"$s34conditional_conformance_with_assoc6IsBothVAA2P2AAWP", i32 0, i32 0), i8*** [[B_AT2_P2_PTR]], align 8
// CHECK-NEXT:    [[B_AT2_AT2_AT3_P3_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 2
// CHECK-NEXT:    store i8** getelementptr inbounds ([2 x i8*], [2 x i8*]* @"$s34conditional_conformance_with_assoc4IsP3VAA0F0AAWP", i32 0, i32 0), i8*** [[B_AT2_AT2_AT3_P3_PTR]], align 8
// CHECK-NEXT:    [[Double_P1:%.*]] = call i8** @swift_getWitnessTable
// CHECK-NEXT:    store atomic i8** [[Double_P1]], i8*** @"$s34conditional_conformance_with_assoc6DoubleVyAA8IsAlsoP2VAA0F2P3VGACyxq_GAA2P1A2A0I0R_AA0H03AT2RpzAakM_AmaLP3AT3RPzrlWL" release, align 8
// CHECK-NEXT:    br label %cont

// CHECK:       cont:
// CHECK-NEXT:    [[T0:%.*]] = phi i8** [ [[CACHE]], %entry ], [ [[Double_P1]], %cacheIsNull ]
// CHECK-NEXT:    ret i8** [[T0]]
// CHECK-NEXT:  }



// witness table instantiator for Double : P1

// CHECK-LABEL: define internal void @"$s34conditional_conformance_with_assoc6DoubleVyxq_GAA2P1A2A2P3R_AA2P23AT2RpzAafH_AhaGP3AT3RPzrlWI"(i8**, %swift.type* %"Double<B, C>", i8**)
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TABLES:%.*]] = bitcast i8** %1 to i8***

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
// CHECK-NEXT:  }

protocol Base {
}

protocol Sub : Base {
  associatedtype S : Base
}

struct X<T> {
}

extension X: Base where T: Base { }
extension X: Sub where T: Sub, T.S == T {
   typealias S = X<T>
}

// Make sure we can recover the type metadata from X<T>.Type.
// CHECK: define internal void @"$s34conditional_conformance_with_assoc1XVyxGAA3SubA2aERz1SQzRszlWI"(i8**, %swift.type* %"X<T>", i8**)
// CHECK: entry:
// CHECK:   [[XT_TYPE:%.*]] = bitcast %swift.type* %"X<T>" to %swift.type**
// CHECK:   [[ADDR:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[XT_TYPE]], i64 2
// CHECK:   [[T:%.*]] = load %swift.type*, %swift.type** [[ADDR]]
// CHECK:   %T.Base = call swiftcc i8** @swift_getAssociatedConformanceWitness(i8** {{.*}}, %swift.type* %T, %swift.type* %T
// CHECK:   ret void
// CHECK: }
