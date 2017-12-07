public func takes_p1<T: P1>(_: T.Type) {}
public protocol P1 {
  func normal()
  func generic<T: P3>(_: T)
}
public protocol P2 {}
public protocol P3 {}

public struct IsP2: P2 {}

public class Base<A> {}
extension Base: P1 where A: P2 {
  public func normal() {}
  public func generic<T: P3>(_: T) {}
}

// witness method for Base.normal

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @_T032conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlAaEP6normalyyFTW(%T32conditional_conformance_subclass4BaseC.0** noalias nocapture swiftself dereferenceable(8), %swift.type* %Self, i8** %SelfWitnessTable) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[A_P2_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -1
// CHECK-NEXT:    [[A_P2:%.*]] = load i8*, i8** [[A_P2_PTR]], align 8
// CHECK-NEXT:    %"\CF\84_0_0.P2" = bitcast i8* [[A_P2]] to i8**
// CHECK-NEXT:    [[SELF:%.]] = load %T32conditional_conformance_subclass4BaseC.0*, %T32conditional_conformance_subclass4BaseC.0** %0
// CHECK-NEXT:    call swiftcc void @_T032conditional_conformance_subclass4BaseCA2A2P2RzlE6normalyyF(i8** %"\CF\84_0_0.P2", %T32conditional_conformance_subclass4BaseC.0* swiftself [[SELF]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// witness method for Base.generic

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @_T032conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlAaEP7genericyqd__AA2P3Rd__lFTW(%swift.opaque* noalias nocapture, %swift.type* %"\CF\84_1_0", i8** %"\CF\84_1_0.P3", %T32conditional_conformance_subclass4BaseC.1** noalias nocapture swiftself dereferenceable(8), %swift.type* %Self, i8** %SelfWitnessTable) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[A_P2_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -1
// CHECK-NEXT:    [[A_P2:%.*]] = load i8*, i8** [[A_P2_PTR]], align 8
// CHECK-NEXT:    %"\CF\84_0_0.P2" = bitcast i8* [[A_P2]] to i8**
// CHECK-NEXT:    [[SELF:%.]] = load %T32conditional_conformance_subclass4BaseC.1*, %T32conditional_conformance_subclass4BaseC.1** %1, align 8
// CHECK-NEXT:    call swiftcc void @_T032conditional_conformance_subclass4BaseCA2A2P2RzlE7genericyqd__AA2P3Rd__lF(%swift.opaque* noalias nocapture %0, %swift.type* %"\CF\84_1_0", i8** %"\CF\84_0_0.P2", i8** %"\CF\84_1_0.P3", %T32conditional_conformance_subclass4BaseC.1* swiftself [[SELF]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }


public class SubclassGeneric<T>: Base<T> {}
public class SubclassConcrete: Base<IsP2> {}
public class SubclassGenericConcrete: SubclassGeneric<IsP2> {}

public func subclassgeneric_generic<T: P2>(_: T.Type) {
  takes_p1(SubclassGeneric<T>.self)
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T032conditional_conformance_subclass23subclassgeneric_genericyxmAA2P2RzlF(%swift.type*, %swift.type* %T, i8** %T.P2) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [1 x i8**], align 8
// CHECK-NEXT:    [[SubclassGeneric_TYPE:%.*]] = call %swift.type* @_T032conditional_conformance_subclass15SubclassGenericCMa(%swift.type* %T)
// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [1 x i8**], [1 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[T_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** %T.P2, i8*** [[T_P2_PTR]], align 8
// CHECK-NEXT:    [[Base_P1:%.*]] = call i8** @_T032conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlWa(%swift.type* [[SubclassGeneric_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]], i64 1)
// CHECK-NEXT:    call swiftcc void @_T032conditional_conformance_subclass8takes_p1yxmAA2P1RzlF(%swift.type* [[SubclassGeneric_TYPE]], %swift.type* [[SubclassGeneric_TYPE]], i8** [[Base_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// witness table accessor for Base : P1

// CHECK-LABEL: define{{( protected)?}} i8** @_T032conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlWa(%swift.type*, i8***, i64) #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.tables = alloca %swift.witness_table_slice, align 8
// CHECK-NEXT:    [[TABLES_PTR:%.*]] = getelementptr inbounds %swift.witness_table_slice, %swift.witness_table_slice* %conditional.tables, i32 0, i32 0
// CHECK-NEXT:    store i8*** %1, i8**** [[TABLES_PTR]], align 8
// CHECK-NEXT:    [[COUNT_PTR:%.*]] = getelementptr inbounds %swift.witness_table_slice, %swift.witness_table_slice* %conditional.tables, i32 0, i32 1
// CHECK-NEXT:    store i64 %2, i64* [[COUNT_PTR]], align 8
// CHECK-NEXT:    [[CAST_CONDITIONAL_TABLES:%.*]] = bitcast %swift.witness_table_slice* %conditional.tables to i8**
// CHECK-NEXT:    [[TABLE:%.*]] = call i8** @swift_rt_swift_getGenericWitnessTable(%swift.generic_witness_table_cache* @_T032conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlWG, %swift.type* %0, i8** [[CAST_CONDITIONAL_TABLES]])
// CHECK-NEXT:    ret i8** [[TABLE]]
// CHECK-NEXT:  }

public func subclassgeneric_concrete() {
  takes_p1(SubclassGeneric<IsP2>.self)
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T032conditional_conformance_subclass24subclassgeneric_concreteyyF() #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[SubclassGeneric_TYPE:%.*]] = call %swift.type* @_T032conditional_conformance_subclass15SubclassGenericCyAA4IsP2VGMa()
// CHECK-NEXT:    [[Base_P1:%.*]] = call i8** @_T032conditional_conformance_subclass15SubclassGenericCyAA4IsP2VGAA4BaseCyxGAA2P1A2A0G0RzlWl()
// CHECK-NEXT:    call swiftcc void @_T032conditional_conformance_subclass8takes_p1yxmAA2P1RzlF(%swift.type* [[SubclassGeneric_TYPE]], %swift.type* [[SubclassGeneric_TYPE]], i8** [[Base_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// Lazy witness table accessor for the concrete SubclassGeneric<IsP2> : Base.

// CHECK-LABEL: define linkonce_odr hidden i8** @_T032conditional_conformance_subclass15SubclassGenericCyAA4IsP2VGAA4BaseCyxGAA2P1A2A0G0RzlWl() #2 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [1 x i8**], align 8
// CHECK-NEXT:    [[CACHE:%.*]] = load i8**, i8*** @_T032conditional_conformance_subclass15SubclassGenericCyAA4IsP2VGAA4BaseCyxGAA2P1A2A0G0RzlWL, align 8
// CHECK-NEXT:    [[IS_NULL:%.*]] = icmp eq i8** [[CACHE]], null
// CHECK-NEXT:    br i1 [[IS_NULL]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:    [[SubclassGeneric_TYPE:%.*]] = call %swift.type* @_T032conditional_conformance_subclass15SubclassGenericCyAA4IsP2VGMa()
// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [1 x i8**], [1 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[A_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** getelementptr inbounds ([0 x i8*], [0 x i8*]* @_T032conditional_conformance_subclass4IsP2VAA0E0AAWP, i32 0, i32 0), i8*** [[A_P2_PTR]], align 8
// CHECK-NEXT:    [[Base_P1:%.*]] = call i8** @_T032conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlWa(%swift.type* [[SubclassGeneric_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]], i64 1)
// CHECK-NEXT:    store atomic i8** [[Base_P1]], i8*** @_T032conditional_conformance_subclass15SubclassGenericCyAA4IsP2VGAA4BaseCyxGAA2P1A2A0G0RzlWL release, align 8
// CHECK-NEXT:    br label %cont

// CHECK:       cont:
// CHECK-NEXT:    %6 = phi i8** [ [[CACHE]], %entry ], [ [[Base_P1]], %cacheIsNull ]
// CHECK-NEXT:    ret i8** %6
// CHECK-NEXT:  }

public func subclassconcrete() {
  takes_p1(SubclassConcrete.self)
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T032conditional_conformance_subclass16subclassconcreteyyF() #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[SubclassConcrete_TYPE:%.*]] = call %swift.type* @_T032conditional_conformance_subclass16SubclassConcreteCMa()
// CHECK-NEXT:    [[SubclassConcrete_P1:%.*]] = call i8** @_T032conditional_conformance_subclass16SubclassConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWl()
// CHECK-NEXT:    call swiftcc void @_T032conditional_conformance_subclass8takes_p1yxmAA2P1RzlF(%swift.type* [[SubclassConcrete_TYPE]], %swift.type* [[SubclassConcrete_TYPE]], i8** [[SubclassConcrete_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// CHECK-LABEL: define linkonce_odr hidden i8** @_T032conditional_conformance_subclass16SubclassConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWl() #2 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [1 x i8**], align 8
// CHECK-NEXT:    [[CACHE:%.*]] = load i8**, i8*** @_T032conditional_conformance_subclass16SubclassConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWL, align 8
// CHECK-NEXT:    [[IS_NULL:%.*]] = icmp eq i8** [[CACHE]], null
// CHECK-NEXT:    br i1 [[IS_NULL]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:    [[SubclassConcrete_TYPE:%.*]] = call %swift.type* @_T032conditional_conformance_subclass16SubclassConcreteCMa()
// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [1 x i8**], [1 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[A_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** getelementptr inbounds ([0 x i8*], [0 x i8*]* @_T032conditional_conformance_subclass4IsP2VAA0E0AAWP, i32 0, i32 0), i8*** [[A_P2_PTR]], align 8
// CHECK-NEXT:    [[Base_P1:%.*]] = call i8** @_T032conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlWa(%swift.type* [[SubclassGeneric_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]], i64 1)
// CHECK-NEXT:    store atomic i8** [[Base_P1]], i8*** @_T032conditional_conformance_subclass16SubclassConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWL release, align 8
// CHECK-NEXT:    br label %cont

// CHECK:       cont:
// CHECK-NEXT:    %6 = phi i8** [ [[CACHE]], %entry ], [ [[Base_P1]], %cacheIsNull ]
// CHECK-NEXT:    ret i8** %6
// CHECK-NEXT:  }

public func subclassgenericconcrete() {
  takes_p1(SubclassGenericConcrete.self)
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T032conditional_conformance_subclass23subclassgenericconcreteyyF() #0 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[SubclassGenericConcrete_TYPE:%.*]] = call %swift.type* @_T032conditional_conformance_subclass23SubclassGenericConcreteCMa()
// CHECK-NEXT:    [[SubclassGenericConcrete_P1:%.*]] = call i8** @_T032conditional_conformance_subclass23SubclassGenericConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWl()
// CHECK-NEXT:    call swiftcc void @_T032conditional_conformance_subclass8takes_p1yxmAA2P1RzlF(%swift.type* [[SubclassGenericConcrete_TYPE]], %swift.type* [[SubclassGenericConcrete_TYPE]], i8** [[SubclassGenericConcrete_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// CHECK-LABEL: define linkonce_odr hidden i8** @_T032conditional_conformance_subclass23SubclassGenericConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWl() #2 {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [1 x i8**], align 8
// CHECK-NEXT:    [[CACHE:%.*]] = load i8**, i8*** @_T032conditional_conformance_subclass23SubclassGenericConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWL, align 8
// CHECK-NEXT:    [[IS_NULL:%.*]] = icmp eq i8** [[CACHE]], null
// CHECK-NEXT:    br i1 [[IS_NULL]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:    [[SubclassGenericConcrete_TYPE:%.*]] = call %swift.type* @_T032conditional_conformance_subclass23SubclassGenericConcreteCMa()
// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [1 x i8**], [1 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[A_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** getelementptr inbounds ([0 x i8*], [0 x i8*]* @_T032conditional_conformance_subclass4IsP2VAA0E0AAWP, i32 0, i32 0), i8*** [[A_P2_PTR]], align 8
// CHECK-NEXT:    [[Base_P1:%.*]] = call i8** @_T032conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlWa(%swift.type* [[SubclassGeneric_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]], i64 1)
// CHECK-NEXT:    store atomic i8** [[Base_P1]], i8*** @_T032conditional_conformance_subclass23SubclassGenericConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWL release, align 8
// CHECK-NEXT:    br label %cont

// CHECK:       cont:
// CHECK-NEXT:    %6 = phi i8** [ [[CACHE]], %entry ], [ [[Base_P1]], %cacheIsNull ]
// CHECK-NEXT:    ret i8** %6
// CHECK-NEXT:  }


// witness tabel instantiation function for Base : P1

// CHECK-LABEL: define internal void @_T032conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlWI(i8**, %swift.type*, i8**) #0 {
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


