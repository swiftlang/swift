// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir > %t.ll
// RUN: %FileCheck %s -check-prefix=GLOBAL < %t.ll
// RUN: %FileCheck %s < %t.ll

// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir -wmo -num-threads 1 > %t.ll.wmo
// RUN: %FileCheck %s -check-prefix=GLOBAL < %t.ll.wmo
// RUN: %FileCheck %s < %t.ll.wmo
// REQUIRES: CPU=x86_64

protocol P {}
protocol Q {}

protocol Assocked {
  associatedtype Assoc : P, Q
}

struct Universal : P, Q {}


// CHECK-LABEL: @"symbolic _____ 23associated_type_witness12OuterPrivate{{.*}}V" = linkonce_odr hidden constant
// CHECK-SAME: @"$s23associated_type_witness12OuterPrivate{{.*}}5InnerE0V9InnermostVMn"
private struct OuterPrivate {
  struct InnerPrivate: HasSimpleAssoc {
    struct Innermost { }
    typealias Assoc = Innermost
  }
}

// CHECK: [[ASSOC_TYPE_NAMES:@.*]] = private constant [29 x i8] c"OneAssoc TwoAssoc ThreeAssoc\00"
// CHECK: @"$s23associated_type_witness18HasThreeAssocTypesMp" =
// CHECK-SAME: [[ASSOC_TYPE_NAMES]] to i64

protocol HasThreeAssocTypes {
  associatedtype OneAssoc
  associatedtype TwoAssoc
  associatedtype ThreeAssoc
}

//   Witness table access functions for Universal : P and Universal : Q.
// CHECK-LABEL: define hidden i8** @"$s23associated_type_witness9UniversalVAA1PAAWa"()
// CHECK:         ret i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @"$s23associated_type_witness9UniversalVAA1PAAWP", i32 0, i32 0)
// CHECK-LABEL: define hidden i8** @"$s23associated_type_witness9UniversalVAA1QAAWa"()
// CHECK:         ret i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @"$s23associated_type_witness9UniversalVAA1QAAWP", i32 0, i32 0)

//   Witness table for WithUniversal : Assocked.
// GLOBAL-LABEL: @"$s23associated_type_witness13WithUniversalVAA8AssockedAAWP" = hidden global [4 x i8*] [
// GLOBAL-SAME:    @"$s23associated_type_witness13WithUniversalVAA8AssockedAAMc"
// GLOBAL-SAME:    i8* bitcast (i8** ()* @"$s23associated_type_witness9UniversalVAA1PAAWa" to i8*)
// GLOBAL-SAME:    i8* bitcast (i8** ()* @"$s23associated_type_witness9UniversalVAA1QAAWa" to i8*)  
// GLOBAL-SAME:    i64 add (i64 ptrtoint (<{ [36 x i8], i8 }>* @"symbolic 23associated_type_witness9UniversalV" to i64), i64 1) to i8*)
// GLOBAL-SAME:  ]
struct WithUniversal : Assocked {
  typealias Assoc = Universal
}

//   Witness table for GenericWithUniversal : Assocked.
// GLOBAL-LABEL: @"$s23associated_type_witness20GenericWithUniversalVyxGAA8AssockedAAWP" = hidden global [4 x i8*] [
// GLOBAL-SAME:    @"$s23associated_type_witness20GenericWithUniversalVyxGAA8AssockedAAMc"
// GLOBAL-SAME:    i8* bitcast (i8** ()* @"$s23associated_type_witness9UniversalVAA1PAAWa" to i8*)
// GLOBAL-SAME:    i8* bitcast (i8** ()* @"$s23associated_type_witness9UniversalVAA1QAAWa" to i8*)  
// GLOBAL-SAME:    @"symbolic 23associated_type_witness9UniversalV"
// GLOBAL-SAME:  ]
struct GenericWithUniversal<T> : Assocked {
  typealias Assoc = Universal
}

//   Witness table for Fulfilled : Assocked.
// GLOBAL-LABEL: @"$s23associated_type_witness9FulfilledVyxGAA8AssockedAAWp" = internal constant [4 x i8*] [
// GLOBAL-SAME:    @"$s23associated_type_witness9FulfilledVyxGAA8AssockedAAMc"
// GLOBAL-SAME:    i8* bitcast (i8** (%swift.type*, %swift.type*, i8**)* @"$s23associated_type_witness9FulfilledVyxGAA8AssockedAA5AssocAaEP_AA1PPWT" to i8*)
// GLOBAL-SAME:    i8* bitcast (i8** (%swift.type*, %swift.type*, i8**)* @"$s23associated_type_witness9FulfilledVyxGAA8AssockedAA5AssocAaEP_AA1QPWT" to i8*)
// GLOBAL-SAME:    @"symbolic x"
// GLOBAL-SAME:  ]
struct Fulfilled<T : P & Q> : Assocked {
  typealias Assoc = T
}

//   Associated type witness table access function for Fulfilled.Assoc : P.
// CHECK-LABEL:  define internal swiftcc i8** @"$s23associated_type_witness9FulfilledVyxGAA8AssockedAA5AssocAaEP_AA1PPWT"(%swift.type* %"Fulfilled<T>.Assoc", %swift.type* %"Fulfilled<T>", i8** %"Fulfilled<T>.Assocked")
// CHECK:         [[T0:%.*]] = bitcast %swift.type* %"Fulfilled<T>" to i8***
// CHECK-NEXT:    [[T1:%.*]] = getelementptr inbounds i8**, i8*** [[T0]], i64 3
// CHECK-NEXT:    [[T2:%.*]] = load i8**, i8*** [[T1]], align 8, !invariant.load
// CHECK-NEXT:    ret i8** [[T2]]

//   Associated type witness table access function for Fulfilled.Assoc : Q.
// CHECK-LABEL:  define internal swiftcc i8** @"$s23associated_type_witness9FulfilledVyxGAA8AssockedAA5AssocAaEP_AA1QPWT"(%swift.type* %"Fulfilled<T>.Assoc", %swift.type* %"Fulfilled<T>", i8** %"Fulfilled<T>.Assocked")
// CHECK:         [[T0:%.*]] = bitcast %swift.type* %"Fulfilled<T>" to i8***
// CHECK-NEXT:    [[T1:%.*]] = getelementptr inbounds i8**, i8*** [[T0]], i64 4
// CHECK-NEXT:    [[T2:%.*]] = load i8**, i8*** [[T1]], align 8, !invariant.load
// CHECK-NEXT:    ret i8** [[T2]]

struct Pair<T, U> : P, Q {}

//   Generic witness table pattern for Computed : Assocked.
// GLOBAL-LABEL: @"$s23associated_type_witness8ComputedVyxq_GAA8AssockedAAWp" = internal constant [4 x i8*] [
// GLOBAL-SAME:    @"$s23associated_type_witness8ComputedVyxq_GAA8AssockedAAMc"
// GLOBAL-SAME:    i8* bitcast (i8** (%swift.type*, %swift.type*, i8**)* @"$s23associated_type_witness8ComputedVyxq_GAA8AssockedAA5Assoc_AA1PPWT" to i8*)
// GLOBAL-SAME:    i8* bitcast (i8** (%swift.type*, %swift.type*, i8**)* @"$s23associated_type_witness8ComputedVyxq_GAA8AssockedAA5Assoc_AA1QPWT" to i8*)
// GLOBAL-SAME:    @"symbolic 23associated_type_witness4PairVyxq_G"
// GLOBAL-SAME:  ]
//   Generic witness table cache for Computed : Assocked.
// GLOBAL-LABEL: @"$s23associated_type_witness8ComputedVyxq_GAA8AssockedAAWG" = internal constant %swift.generic_witness_table_cache {
// GLOBAL-SAME:    i16 4,
// GLOBAL-SAME:    i16 1,

//    Relative reference to witness table template
// GLOBAL-SAME:    i32 trunc (i64 sub (i64 ptrtoint ([4 x i8*]* @"$s23associated_type_witness8ComputedVyxq_GAA8AssockedAAWp" to i64), i64 ptrtoint (i32* getelementptr inbounds (%swift.generic_witness_table_cache, %swift.generic_witness_table_cache* @"$s23associated_type_witness8ComputedVyxq_GAA8AssockedAAWG", i32 0, i32 2) to i64)) to i32

//    No instantiator function
// GLOBAL-SAME:    i32 0,
// GLOBAL-SAME:    i32 trunc (i64 sub (i64 ptrtoint ([16 x i8*]* [[PRIVATE:@.*]] to i64), i64 ptrtoint (i32* getelementptr inbounds (%swift.generic_witness_table_cache, %swift.generic_witness_table_cache* @"$s23associated_type_witness8ComputedVyxq_GAA8AssockedAAWG", i32 0, i32 4) to i64)) to i32)
// GLOBAL-SAME:  }
// GLOBAL:       [[PRIVATE]] = internal global [16 x i8*] zeroinitializer

struct Computed<T, U> : Assocked {
  typealias Assoc = Pair<T, U>
}

//   Witness table accessor function for Computed : Assocked.
// CHECK-LABEL: define hidden i8** @"$s23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWa"(%swift.type*, i8***)
// CHECK-NEXT:  entry:
// CHECK-NEXT:     [[WTABLE:%.*]] = call i8** @swift_getGenericWitnessTable(%swift.generic_witness_table_cache* @"$s23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWG", %swift.type* %0, i8*** %1)
// CHECK-NEXT:     ret i8** [[WTABLE]]


struct PBox<T: P> {}
protocol HasSimpleAssoc {
  associatedtype Assoc
}
protocol DerivedFromSimpleAssoc : HasSimpleAssoc {}

//   Generic witness table pattern for GenericComputed : DerivedFromSimpleAssoc.
// GLOBAL-LABEL: @"$s23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWp" = internal constant [2 x i8*]
// GLOBAL-SAME: @"$s23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAMc"
// GLOBAL-SAME: i8* null

//   Generic witness table cache for GenericComputed : DerivedFromSimpleAssoc.
// GLOBAL-LABEL: @"$s23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWG" = internal constant %swift.generic_witness_table_cache {
// GLOBAL-SAME:    i16 2,
// GLOBAL-SAME:    i16 1,

//   Relative reference to witness table template
// GLOBAL-SAME:    i32 trunc (i64 sub (i64 ptrtoint ([2 x i8*]* @"$s23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWp" to i64

//   Relative reference to instantiator function
// GLOBAL-SAME:    i32 trunc (i64 sub (i64 ptrtoint (void (i8**, %swift.type*, i8**)* @"$s23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWI" to i64), i64 ptrtoint (i32* getelementptr inbounds (%swift.generic_witness_table_cache, %swift.generic_witness_table_cache* @"$s23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWG", i32 0, i32 3) to i64)) to i32)

//   Relative reference to private data
struct GenericComputed<T: P> : DerivedFromSimpleAssoc {
  typealias Assoc = PBox<T>
}

//   Instantiation function for GenericComputed : DerivedFromSimpleAssoc.
// CHECK-LABEL: define internal void @"$s23associated_type_witness15GenericComputedVyxGAA22DerivedFromSimpleAssocAAWI"(i8**, %swift.type* %"GenericComputed<T>", i8**)
// CHECK:         [[T0:%.*]] = call i8** @"$s23associated_type_witness15GenericComputedVyxGAA14HasSimpleAssocAAWa"(%swift.type* %"GenericComputed<T>", i8*** undef)
// CHECK-NEXT:    [[T1:%.*]] = bitcast i8** [[T0]] to i8*
// CHECK-NEXT:    [[T2:%.*]] = getelementptr inbounds i8*, i8** %0, i32 1
// CHECK-NEXT:    store i8* [[T1]], i8** [[T2]], align 8
// CHECK-NEXT:    ret void

//   Witness table accessor function for GenericComputed : HasSimpleAssoc..
// CHECK-LABEL: define hidden i8** @"$s23associated_type_witness15GenericComputedVyxGAA14HasSimpleAssocAAWa"(%swift.type*, i8***)
// CHECK-NEXT:   entry:
// CHECK-NEXT:    [[WTABLE:%.*]] = call i8** @swift_getGenericWitnessTable(%swift.generic_witness_table_cache* @"$s23associated_type_witness15GenericComputedVyxGAA14HasSimpleAssocAAWG", %swift.type* %0, i8*** %1)
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
