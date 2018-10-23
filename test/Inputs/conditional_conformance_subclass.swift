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

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @"$s32conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlAaEP6normalyyFTW"(%T32conditional_conformance_subclass4BaseC.0** noalias nocapture swiftself dereferenceable(8), %swift.type* %Self, i8** %SelfWitnessTable)
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[A_P2_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -1
// CHECK-NEXT:    [[A_P2:%.*]] = load i8*, i8** [[A_P2_PTR]], align 8
// CHECK-NEXT:    %"\CF\84_0_0.P2" = bitcast i8* [[A_P2]] to i8**
// CHECK-NEXT:    [[SELF:%.]] = load %T32conditional_conformance_subclass4BaseC.0*, %T32conditional_conformance_subclass4BaseC.0** %0
// CHECK-NEXT:    call swiftcc void @"$s32conditional_conformance_subclass4BaseCA2A2P2RzlE6normalyyF"(i8** %"\CF\84_0_0.P2", %T32conditional_conformance_subclass4BaseC.0* swiftself [[SELF]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// witness method for Base.generic

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @"$s32conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlAaEP7genericyyqd__AA2P3Rd__lFTW"(%swift.opaque* noalias nocapture, %swift.type* %"\CF\84_1_0", i8** %"\CF\84_1_0.P3", %T32conditional_conformance_subclass4BaseC.1** noalias nocapture swiftself dereferenceable(8), %swift.type* %Self, i8** %SelfWitnessTable)
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[A_P2_PTR:%.*]] = getelementptr inbounds i8*, i8** %SelfWitnessTable, i32 -1
// CHECK-NEXT:    [[A_P2:%.*]] = load i8*, i8** [[A_P2_PTR]], align 8
// CHECK-NEXT:    %"\CF\84_0_0.P2" = bitcast i8* [[A_P2]] to i8**
// CHECK-NEXT:    [[SELF:%.]] = load %T32conditional_conformance_subclass4BaseC.1*, %T32conditional_conformance_subclass4BaseC.1** %1, align 8
// CHECK-NEXT:    call swiftcc void @"$s32conditional_conformance_subclass4BaseCA2A2P2RzlE7genericyyqd__AA2P3Rd__lF"(%swift.opaque* noalias nocapture %0, %swift.type* %"\CF\84_1_0", i8** %"\CF\84_0_0.P2", i8** %"\CF\84_1_0.P3", %T32conditional_conformance_subclass4BaseC.1* swiftself [[SELF]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }


public class SubclassGeneric<T>: Base<T> {}
public class SubclassConcrete: Base<IsP2> {}
public class SubclassGenericConcrete: SubclassGeneric<IsP2> {}

public func subclassgeneric_generic<T: P2>(_: T.Type) {
  takes_p1(SubclassGeneric<T>.self)
}

// CHECK-LABEL: define{{( dllexport| protected)?}} swiftcc void @"$s32conditional_conformance_subclass23subclassgeneric_genericyyxmAA2P2RzlF"(%swift.type*, %swift.type* %T, i8** %T.P2)
// CHECK-NEXT:  entry:
// CHECK:         %conditional.requirement.buffer = alloca [1 x i8**], align 8
// CHECK:         [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s32conditional_conformance_subclass15SubclassGenericCMa"(i64 0, %swift.type* %T)
// CHECK-NEXT:    [[SubclassGeneric_TYPE:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [1 x i8**], [1 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[T_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** %T.P2, i8*** [[T_P2_PTR]], align 8
// CHECK-NEXT:    [[Base_P1:%.*]] = call i8** @"$s32conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlWa"(%swift.type* [[SubclassGeneric_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]])
// CHECK-NEXT:    call swiftcc void @"$s32conditional_conformance_subclass8takes_p1yyxmAA2P1RzlF"(%swift.type* [[SubclassGeneric_TYPE]], %swift.type* [[SubclassGeneric_TYPE]], i8** [[Base_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// witness table accessor for Base : P1

// CHECK-LABEL: define{{( dllexport| protected)?}} i8** @"$s32conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlWa"(%swift.type*, i8***)
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TABLE:%.*]] = call i8** @swift_instantiateWitnessTable(%swift.protocol_conformance_descriptor* bitcast{{.*}}@"$s32conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlMc"{{.*}}, %swift.type* %0, i8*** %1)
// CHECK-NEXT:    ret i8** [[TABLE]]
// CHECK-NEXT:  }

public func subclassgeneric_concrete() {
  takes_p1(SubclassGeneric<IsP2>.self)
}

// CHECK-LABEL: define{{( dllexport| protected)?}} swiftcc void @"$s32conditional_conformance_subclass24subclassgeneric_concreteyyF"()
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s32conditional_conformance_subclass15SubclassGenericCyAA4IsP2VGMa"(i64 0)
// CHECK-NEXT:    [[SubclassGeneric_TYPE:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT:    [[Base_P1:%.*]] = call i8** @"$s32conditional_conformance_subclass15SubclassGenericCyAA4IsP2VGAA4BaseCyxGAA2P1A2A0G0RzlWl"()
// CHECK-NEXT:    call swiftcc void @"$s32conditional_conformance_subclass8takes_p1yyxmAA2P1RzlF"(%swift.type* [[SubclassGeneric_TYPE]], %swift.type* [[SubclassGeneric_TYPE]], i8** [[Base_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// Lazy witness table accessor for the concrete SubclassGeneric<IsP2> : Base.

// CHECK-LABEL: define linkonce_odr hidden i8** @"$s32conditional_conformance_subclass15SubclassGenericCyAA4IsP2VGAA4BaseCyxGAA2P1A2A0G0RzlWl"()
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [1 x i8**], align 8
// CHECK-NEXT:    [[CACHE:%.*]] = load i8**, i8*** @"$s32conditional_conformance_subclass15SubclassGenericCyAA4IsP2VGAA4BaseCyxGAA2P1A2A0G0RzlWL", align 8
// CHECK-NEXT:    [[IS_NULL:%.*]] = icmp eq i8** [[CACHE]], null
// CHECK-NEXT:    br i1 [[IS_NULL]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:    [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s32conditional_conformance_subclass15SubclassGenericCyAA4IsP2VGMa"(i64 0)
// CHECK-NEXT:    [[SubclassGeneric_TYPE:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [1 x i8**], [1 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[A_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @"$s32conditional_conformance_subclass4IsP2VAA0E0AAWP", i32 0, i32 0), i8*** [[A_P2_PTR]], align 8
// CHECK-NEXT:    [[Base_P1:%.*]] = call i8** @"$s32conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlWa"(%swift.type* [[SubclassGeneric_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]])
// CHECK-NEXT:    store atomic i8** [[Base_P1]], i8*** @"$s32conditional_conformance_subclass15SubclassGenericCyAA4IsP2VGAA4BaseCyxGAA2P1A2A0G0RzlWL" release, align 8
// CHECK-NEXT:    br label %cont

// CHECK:       cont:
// CHECK-NEXT:    [[T0:%.*]] = phi i8** [ [[CACHE]], %entry ], [ [[Base_P1]], %cacheIsNull ]
// CHECK-NEXT:    ret i8** [[T0]]
// CHECK-NEXT:  }

public func subclassconcrete() {
  takes_p1(SubclassConcrete.self)
}

// CHECK-LABEL: define{{( dllexport| protected)?}} swiftcc void @"$s32conditional_conformance_subclass16subclassconcreteyyF"()
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s32conditional_conformance_subclass16SubclassConcreteCMa"(i64 0)
// CHECK-NEXT:    [[SubclassConcrete_TYPE:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT:    [[SubclassConcrete_P1:%.*]] = call i8** @"$s32conditional_conformance_subclass16SubclassConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWl"()
// CHECK-NEXT:    call swiftcc void @"$s32conditional_conformance_subclass8takes_p1yyxmAA2P1RzlF"(%swift.type* [[SubclassConcrete_TYPE]], %swift.type* [[SubclassConcrete_TYPE]], i8** [[SubclassConcrete_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// CHECK-LABEL: define linkonce_odr hidden i8** @"$s32conditional_conformance_subclass16SubclassConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWl"()
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [1 x i8**], align 8
// CHECK-NEXT:    [[CACHE:%.*]] = load i8**, i8*** @"$s32conditional_conformance_subclass16SubclassConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWL", align 8
// CHECK-NEXT:    [[IS_NULL:%.*]] = icmp eq i8** [[CACHE]], null
// CHECK-NEXT:    br i1 [[IS_NULL]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:    [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s32conditional_conformance_subclass16SubclassConcreteCMa"(i64 0)
// CHECK-NEXT:    [[SubclassConcrete_TYPE:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [1 x i8**], [1 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[A_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @"$s32conditional_conformance_subclass4IsP2VAA0E0AAWP", i32 0, i32 0), i8*** [[A_P2_PTR]], align 8
// CHECK-NEXT:    [[Base_P1:%.*]] = call i8** @"$s32conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlWa"(%swift.type* [[SubclassGeneric_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]])
// CHECK-NEXT:    store atomic i8** [[Base_P1]], i8*** @"$s32conditional_conformance_subclass16SubclassConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWL" release, align 8
// CHECK-NEXT:    br label %cont

// CHECK:       cont:
// CHECK-NEXT:    [[T0:%.*]] = phi i8** [ [[CACHE]], %entry ], [ [[Base_P1]], %cacheIsNull ]
// CHECK-NEXT:    ret i8** [[T0]]
// CHECK-NEXT:  }

public func subclassgenericconcrete() {
  takes_p1(SubclassGenericConcrete.self)
}

// CHECK-LABEL: define{{( dllexport| protected)?}} swiftcc void @"$s32conditional_conformance_subclass23subclassgenericconcreteyyF"()
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s32conditional_conformance_subclass23SubclassGenericConcreteCMa"(i64 0)
// CHECK-NEXT:    [[SubclassGenericConcrete_TYPE:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT:    [[SubclassGenericConcrete_P1:%.*]] = call i8** @"$s32conditional_conformance_subclass23SubclassGenericConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWl"()
// CHECK-NEXT:    call swiftcc void @"$s32conditional_conformance_subclass8takes_p1yyxmAA2P1RzlF"(%swift.type* [[SubclassGenericConcrete_TYPE]], %swift.type* [[SubclassGenericConcrete_TYPE]], i8** [[SubclassGenericConcrete_P1]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }

// CHECK-LABEL: define linkonce_odr hidden i8** @"$s32conditional_conformance_subclass23SubclassGenericConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWl"()
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %conditional.requirement.buffer = alloca [1 x i8**], align 8
// CHECK-NEXT:    [[CACHE:%.*]] = load i8**, i8*** @"$s32conditional_conformance_subclass23SubclassGenericConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWL", align 8
// CHECK-NEXT:    [[IS_NULL:%.*]] = icmp eq i8** [[CACHE]], null
// CHECK-NEXT:    br i1 [[IS_NULL]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:    [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s32conditional_conformance_subclass23SubclassGenericConcreteCMa"(i64 0)
// CHECK-NEXT:    [[SubclassGenericConcrete_TYPE:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT:    [[CONDITIONAL_REQUIREMENTS:%.*]] = getelementptr inbounds [1 x i8**], [1 x i8**]* %conditional.requirement.buffer, i32 0, i32 0
// CHECK-NEXT:    [[A_P2_PTR:%.*]] = getelementptr inbounds i8**, i8*** [[CONDITIONAL_REQUIREMENTS]], i32 0
// CHECK-NEXT:    store i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @"$s32conditional_conformance_subclass4IsP2VAA0E0AAWP", i32 0, i32 0), i8*** [[A_P2_PTR]], align 8
// CHECK-NEXT:    [[Base_P1:%.*]] = call i8** @"$s32conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlWa"(%swift.type* [[SubclassGeneric_TYPE]], i8*** [[CONDITIONAL_REQUIREMENTS]])
// CHECK-NEXT:    store atomic i8** [[Base_P1]], i8*** @"$s32conditional_conformance_subclass23SubclassGenericConcreteCAA4BaseCyxGAA2P1A2A2P2RzlWL" release, align 8
// CHECK-NEXT:    br label %cont

// CHECK:       cont:
// CHECK-NEXT:    [[T0:%.*]] = phi i8** [ [[CACHE]], %entry ], [ [[Base_P1]], %cacheIsNull ]
// CHECK-NEXT:    ret i8** [[T0]]
// CHECK-NEXT:  }


// witness tabel instantiation function for Base : P1

// CHECK-LABEL: define internal void @"$s32conditional_conformance_subclass4BaseCyxGAA2P1A2A2P2RzlWI"(i8**, %swift.type* %"Base<A>", i8**)
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TABLES:%.*]] = bitcast i8** %1 to i8***

// CHECK-NEXT:    [[A_P2_SRC:%.*]] = getelementptr inbounds i8**, i8*** [[TABLES]], i32 0
// CHECK-NEXT:    [[A_P2_DEST:%.*]] = getelementptr inbounds i8*, i8** %0, i32 -1
// CHECK-NEXT:    [[A_P2:%.*]] = load i8**, i8*** [[A_P2_SRC]], align 8
// CHECK-NEXT:    [[CAST_A_P2_DEST:%.*]] = bitcast i8** [[A_P2_DEST]] to i8***
// CHECK-NEXT:    store i8** [[A_P2]], i8*** [[CAST_A_P2_DEST]], align 8

// CHECK-NEXT:    ret void
// CHECK-NEXT:  }


