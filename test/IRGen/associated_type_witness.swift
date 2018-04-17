// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir > %t.ll
// RUN: %FileCheck %s -check-prefix=GLOBAL < %t.ll
// RUN: %FileCheck %s < %t.ll
// REQUIRES: CPU=x86_64

protocol P {}
protocol Q {}

protocol Assocked {
  associatedtype Assoc : P, Q
}

struct Universal : P, Q {}

// CHECK: [[ASSOC_TYPE_NAMES:@.*]] = private constant [29 x i8] c"OneAssoc ThreeAssoc TwoAssoc\00"
// CHECK: @"$S23associated_type_witness18HasThreeAssocTypesMp" =
// CHECK-SAME: [[ASSOC_TYPE_NAMES]] to i64

protocol HasThreeAssocTypes {
  associatedtype OneAssoc
  associatedtype TwoAssoc
  associatedtype ThreeAssoc
}

//   Witness table access functions for Universal : P and Universal : Q.
// CHECK-LABEL: define hidden i8** @"$S23associated_type_witness9UniversalVAA1PAAWa"()
// CHECK:         ret i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @"$S23associated_type_witness9UniversalVAA1PAAWP", i32 0, i32 0)
// CHECK-LABEL: define hidden i8** @"$S23associated_type_witness9UniversalVAA1QAAWa"()
// CHECK:         ret i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @"$S23associated_type_witness9UniversalVAA1QAAWP", i32 0, i32 0)

//   Witness table for WithUniversal : Assocked.
// GLOBAL-LABEL: @"$S23associated_type_witness13WithUniversalVAA8AssockedAAWP" = hidden constant [4 x i8*] [
// GLOBAL-SAME:    @"$S23associated_type_witness13WithUniversalVAA8AssockedAAMc"
// GLOBAL-SAME:    i8* bitcast (%swift.metadata_response (i64)* @"$S23associated_type_witness9UniversalVMa" to i8*)
// GLOBAL-SAME:    i8* bitcast (i8** ()* @"$S23associated_type_witness9UniversalVAA1PAAWa" to i8*)
// GLOBAL-SAME:    i8* bitcast (i8** ()* @"$S23associated_type_witness9UniversalVAA1QAAWa" to i8*)  
// GLOBAL-SAME:  ]
struct WithUniversal : Assocked {
  typealias Assoc = Universal
}

//   Witness table for GenericWithUniversal : Assocked.
// GLOBAL-LABEL: @"$S23associated_type_witness20GenericWithUniversalVyxGAA8AssockedAAWP" = hidden constant [4 x i8*] [
// GLOBAL-SAME:    @"$S23associated_type_witness20GenericWithUniversalVyxGAA8AssockedAAMc"
// GLOBAL-SAME:    i8* bitcast (%swift.metadata_response (i64)* @"$S23associated_type_witness9UniversalVMa" to i8*)
// GLOBAL-SAME:    i8* bitcast (i8** ()* @"$S23associated_type_witness9UniversalVAA1PAAWa" to i8*)
// GLOBAL-SAME:    i8* bitcast (i8** ()* @"$S23associated_type_witness9UniversalVAA1QAAWa" to i8*)  
// GLOBAL-SAME:  ]
struct GenericWithUniversal<T> : Assocked {
  typealias Assoc = Universal
}

//   Witness table for Fulfilled : Assocked.
// GLOBAL-LABEL: @"$S23associated_type_witness9FulfilledVyxGAA8AssockedAAWp" = internal constant [4 x i8*] [
// GLOBAL-SAME:    @"$S23associated_type_witness9FulfilledVyxGAA8AssockedAAMc"
// GLOBAL-SAME:    i8* bitcast (%swift.metadata_response (i64, %swift.type*, i8**)* @"$S23associated_type_witness9FulfilledVyxGAA8AssockedAA5AssocWt" to i8*)
// GLOBAL-SAME:    i8* bitcast (i8** (%swift.type*, %swift.type*, i8**)* @"$S23associated_type_witness9FulfilledVyxGAA8AssockedAA5Assoc_AA1PPWT" to i8*)
// GLOBAL-SAME:    i8* bitcast (i8** (%swift.type*, %swift.type*, i8**)* @"$S23associated_type_witness9FulfilledVyxGAA8AssockedAA5Assoc_AA1QPWT" to i8*)
// GLOBAL-SAME:  ]
struct Fulfilled<T : P & Q> : Assocked {
  typealias Assoc = T
}

//   Associated type metadata access function for Fulfilled.Assoc.
// CHECK-LABEL:  define internal swiftcc %swift.metadata_response @"$S23associated_type_witness9FulfilledVyxGAA8AssockedAA5AssocWt"(i64, %swift.type* %"Fulfilled<T>", i8** %"Fulfilled<T>.Assocked")
// CHECK:         [[T0:%.*]] = bitcast %swift.type* %"Fulfilled<T>" to %swift.type**
// CHECK-NEXT:    [[T1:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[T0]], i64 2
// CHECK-NEXT:    [[T2:%.*]] = load %swift.type*, %swift.type** [[T1]], align 8, !invariant.load
// CHECK-NEXT:    [[T3:%.*]] = call swiftcc %swift.metadata_response @swift_checkMetadataState(i64 %0, %swift.type* [[T2]])
// CHECK-NEXT:    [[CHECKED:%.*]] = extractvalue %swift.metadata_response [[T3]], 0
// CHECK-NEXT:    [[STATE:%.*]] = extractvalue %swift.metadata_response [[T3]], 1
// CHECK-NEXT:    [[T3:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[CHECKED]], 0
// CHECK-NEXT:    [[T4:%.*]] = insertvalue %swift.metadata_response [[T3]], i64 [[STATE]], 1
// CHECK-NEXT:    ret %swift.metadata_response [[T4]]

//   Associated type witness table access function for Fulfilled.Assoc : P.
// CHECK-LABEL:  define internal swiftcc i8** @"$S23associated_type_witness9FulfilledVyxGAA8AssockedAA5Assoc_AA1PPWT"(%swift.type* %"Fulfilled<T>.Assoc", %swift.type* %"Fulfilled<T>", i8** %"Fulfilled<T>.Assocked")
// CHECK:         [[T0:%.*]] = bitcast %swift.type* %"Fulfilled<T>" to i8***
// CHECK-NEXT:    [[T1:%.*]] = getelementptr inbounds i8**, i8*** [[T0]], i64 3
// CHECK-NEXT:    [[T2:%.*]] = load i8**, i8*** [[T1]], align 8, !invariant.load
// CHECK-NEXT:    ret i8** [[T2]]

//   Associated type witness table access function for Fulfilled.Assoc : Q.
// CHECK-LABEL:  define internal swiftcc i8** @"$S23associated_type_witness9FulfilledVyxGAA8AssockedAA5Assoc_AA1QPWT"(%swift.type* %"Fulfilled<T>.Assoc", %swift.type* %"Fulfilled<T>", i8** %"Fulfilled<T>.Assocked")
// CHECK:         [[T0:%.*]] = bitcast %swift.type* %"Fulfilled<T>" to i8***
// CHECK-NEXT:    [[T1:%.*]] = getelementptr inbounds i8**, i8*** [[T0]], i64 4
// CHECK-NEXT:    [[T2:%.*]] = load i8**, i8*** [[T1]], align 8, !invariant.load
// CHECK-NEXT:    ret i8** [[T2]]

struct Pair<T, U> : P, Q {}

//   Generic witness table pattern for Computed : Assocked.
// GLOBAL-LABEL: @"$S23associated_type_witness8ComputedVyxq_GAA8AssockedAAWp" = internal constant [4 x i8*] [
// GLOBAL-SAME:    @"$S23associated_type_witness8ComputedVyxq_GAA8AssockedAAMc"
// GLOBAL-SAME:    i8* bitcast (%swift.metadata_response (i64, %swift.type*, i8**)* @"$S23associated_type_witness8ComputedVyxq_GAA8AssockedAA5AssocWt" to i8*)
// GLOBAL-SAME:    i8* bitcast (i8** (%swift.type*, %swift.type*, i8**)* @"$S23associated_type_witness8ComputedVyxq_GAA8AssockedAA5Assoc_AA1PPWT" to i8*)
// GLOBAL-SAME:    i8* bitcast (i8** (%swift.type*, %swift.type*, i8**)* @"$S23associated_type_witness8ComputedVyxq_GAA8AssockedAA5Assoc_AA1QPWT" to i8*)
// GLOBAL-SAME:  ]
//   Generic witness table cache for Computed : Assocked.
// GLOBAL-LABEL: @"$S23associated_type_witness8ComputedVyxq_GAA8AssockedAAWG" = internal constant %swift.generic_witness_table_cache {
// GLOBAL-SAME:    i16 4,
// GLOBAL-SAME:    i16 1,

//    Relative reference to protocol
// GLOBAL-SAME:    i32 trunc (i64 sub (i64 ptrtoint (%swift.protocol* @"$S23associated_type_witness8AssockedMp" to i64), i64 ptrtoint (i32* getelementptr inbounds (%swift.generic_witness_table_cache, %swift.generic_witness_table_cache* @"$S23associated_type_witness8ComputedVyxq_GAA8AssockedAAWG", i32 0, i32 2) to i64)) to i32

//    Relative reference to witness table template
// GLOBAL-SAME:    i32 trunc (i64 sub (i64 ptrtoint ([4 x i8*]* @"$S23associated_type_witness8ComputedVyxq_GAA8AssockedAAWp" to i64), i64 ptrtoint (i32* getelementptr inbounds (%swift.generic_witness_table_cache, %swift.generic_witness_table_cache* @"$S23associated_type_witness8ComputedVyxq_GAA8AssockedAAWG", i32 0, i32 3) to i64)) to i32

//    Relative reference to resilient witnesses
// GLOBAL-SAME:    i32 0,

//    No instantiator function
// GLOBAL-SAME:    i32 0,
// GLOBAL-SAME:    i32 trunc (i64 sub (i64 ptrtoint ([16 x i8*]* [[PRIVATE:@.*]] to i64), i64 ptrtoint (i32* getelementptr inbounds (%swift.generic_witness_table_cache, %swift.generic_witness_table_cache* @"$S23associated_type_witness8ComputedVyxq_GAA8AssockedAAWG", i32 0, i32 6) to i64)) to i32)
// GLOBAL-SAME:  }
// GLOBAL:       [[PRIVATE]] = internal global [16 x i8*] zeroinitializer

struct Computed<T, U> : Assocked {
  typealias Assoc = Pair<T, U>
}

//   Associated type metadata access function for Computed.Assoc.
// CHECK-LABEL:  define internal swiftcc %swift.metadata_response @"$S23associated_type_witness8ComputedVyxq_GAA8AssockedAA5AssocWt"(i64, %swift.type* %"Computed<T, U>", i8** %"Computed<T, U>.Assocked")
// CHECK:         entry:
// CHECK:          [[T0:%.*]] = getelementptr inbounds i8*, i8** %"Computed<T, U>.Assocked", i32 -1
// CHECK-NEXT:     [[CACHE:%.*]] = bitcast i8** [[T0]] to %swift.type**
// CHECK-NEXT:     [[CACHE_RESULT:%.*]] = load %swift.type*, %swift.type** [[CACHE]], align 8
// CHECK-NEXT:     [[T1:%.*]] = icmp eq %swift.type* [[CACHE_RESULT]], null
// CHECK-NEXT:     br i1 [[T1]], label %fetch, label %cont
// CHECK:        cont:
// CHECK-NEXT:     [[T0:%.*]] = phi %swift.type* [ [[CACHE_RESULT]], %entry ], [ [[FETCH_RESULT:%.*]], %fetch ],
// CHECK-SAME:        [ [[FETCH_RESULT]], %is_complete ]
// CHECK-NEXT:     [[T1:%.*]] = phi i64 [ 0, %entry ], [ [[FETCH_STATE:%.*]], %fetch ], [ 0, %is_complete ]
// CHECK-NEXT:     [[T2:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[T0]], 0
// CHECK-NEXT:     [[T3:%.*]] = insertvalue %swift.metadata_response [[T2]], i64 [[T1]], 1
// CHECK-NEXT:     ret %swift.metadata_response [[T3]]
// CHECK:        fetch:
// CHECK-NEXT:    [[T0:%.*]] = bitcast %swift.type* %"Computed<T, U>" to %swift.type**
// CHECK-NEXT:    [[T1:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[T0]], i64 2
// CHECK-NEXT:    [[T:%.*]] = load %swift.type*, %swift.type** [[T1]], align 8, !invariant.load
// CHECK:         [[T0:%.*]] = bitcast %swift.type* %"Computed<T, U>" to %swift.type**
// CHECK-NEXT:    [[T1:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[T0]], i64 3
// CHECK-NEXT:    [[U:%.*]] = load %swift.type*, %swift.type** [[T1]], align 8, !invariant.load
// CHECK-NEXT:    [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S23associated_type_witness4PairVMa"(i64 %0, %swift.type* [[T]], %swift.type* [[U]])
// CHECK-NEXT:    [[FETCH_RESULT]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT:    [[FETCH_STATE]] = extractvalue %swift.metadata_response [[T0]], 1
// CHECK-NEXT:    [[COMPLETE:%.*]] = icmp eq i64 [[FETCH_STATE]], 0
// CHECK-NEXT:    br i1 [[COMPLETE]], label %is_complete, label %cont
// CHECK:       is_complete:
// CHECK-NEXT:    store atomic %swift.type* [[FETCH_RESULT]], %swift.type** [[CACHE]] release, align 8
// CHECK-NEXT:    br label %cont

//   Witness table accessor function for Computed : Assocked.
// CHECK-LABEL: define hidden i8** @"$S23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWa"(%swift.type*, i8***)
// CHECK-NEXT:  entry:
// CHECK-NEXT:     [[WTABLE:%.*]] = call i8** @swift_getGenericWitnessTable(%swift.generic_witness_table_cache* @"$S23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWG", %swift.type* %0, i8*** %1)
// CHECK-NEXT:     ret i8** [[WTABLE]]


struct PBox<T: P> {}
protocol HasSimpleAssoc {
  associatedtype Assoc
}
protocol DerivedFromSimpleAssoc : HasSimpleAssoc {}


//   Generic witness table pattern for GenericComputed : DerivedFromSimpleAssoc.
// GLOBAL-LABEL: @"$S23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWp" = internal constant [2 x i8*]
// GLOBAL-SAME: @"$S23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAMc"
// GLOBAL-SAME: i8* null

//   Generic witness table cache for GenericComputed : DerivedFromSimpleAssoc.
// GLOBAL-LABEL: @"$S23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWG" = internal constant %swift.generic_witness_table_cache {
// GLOBAL-SAME:    i16 2,
// GLOBAL-SAME:    i16 0,

//   Relative reference to protocol
// GLOBAL-SAME:    i32 trunc (i64 sub (i64 ptrtoint (%swift.protocol* @"$S23associated_type_witness22DerivedFromSimpleAssocMp" to i64

//   Relative reference to witness table template
// GLOBAL-SAME:    i32 trunc (i64 sub (i64 ptrtoint ([2 x i8*]* @"$S23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWp" to i64

//   Relative reference to resilient witnesses
// GLOBAL-SAME:    i32 0,

//   Relative reference to instantiator function
// GLOBAL-SAME:    i32 trunc (i64 sub (i64 ptrtoint (void (i8**, %swift.type*, i8**)* @"$S23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWI" to i64), i64 ptrtoint (i32* getelementptr inbounds (%swift.generic_witness_table_cache, %swift.generic_witness_table_cache* @"$S23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWG", i32 0, i32 5) to i64)) to i32)

//   Relative reference to private data
// GLOBAL-SAME:    i32 trunc (i64 sub (i64 ptrtoint ([16 x i8*]* @1 to i64), i64 ptrtoint (i32* getelementptr inbounds (%swift.generic_witness_table_cache, %swift.generic_witness_table_cache* @"$S23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWG", i32 0, i32 6) to i64)) to i32)
// GLOBAL-SAME:  }
struct GenericComputed<T: P> : DerivedFromSimpleAssoc {
  typealias Assoc = PBox<T>
}

//   Instantiation function for GenericComputed : DerivedFromSimpleAssoc.
// CHECK-LABEL: define internal void @"$S23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWI"(i8**, %swift.type*, i8**)
// CHECK:         [[T0:%.*]] = call i8** @"$S23associated_type_witness15GenericComputedVyxGAA14HasSimpleAssocAAWa"(%swift.type* %1, i8*** undef)
// CHECK-NEXT:    [[T1:%.*]] = bitcast i8** [[T0]] to i8*
// CHECK-NEXT:    [[T2:%.*]] = getelementptr inbounds i8*, i8** %0, i32 1
// CHECK-NEXT:    store i8* [[T1]], i8** [[T2]], align 8
// CHECK-NEXT:    ret void

//   Witness table accessor function for GenericComputed : HasSimpleAssoc..
// CHECK-LABEL: define hidden i8** @"$S23associated_type_witness15GenericComputedVyxGAA14HasSimpleAssocAAWa"(%swift.type*, i8***)
// CHECK-NEXT:   entry:
// CHECK-NEXT:    [[WTABLE:%.*]] = call i8** @swift_getGenericWitnessTable(%swift.generic_witness_table_cache* @"$S23associated_type_witness15GenericComputedVyxGAA14HasSimpleAssocAAWG", %swift.type* %0, i8*** %1)
// CHECK-NEXT:    ret i8** [[WTABLE]]


protocol HasAssocked {
  associatedtype Contents : Assocked
}
struct FulfilledFromAssociatedType<T : HasAssocked> : HasSimpleAssoc {
  typealias Assoc = PBox<T.Contents.Assoc>
}

struct UsesVoid : HasSimpleAssoc {
  typealias Assoc = ()
}
