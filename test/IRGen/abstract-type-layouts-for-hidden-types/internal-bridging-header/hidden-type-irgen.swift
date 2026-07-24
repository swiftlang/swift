// Test that a Client importing a Library with -internal-import-bridging-header
// can allocate, copy, and destroy values whose stored properties are hidden.

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes
// REQUIRES: PTRSIZE=64

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-clang -x c %t/Utility.c -S -emit-llvm -o %t/Utility.ll
// RUN: %FileCheck --check-prefix=CLANG-SIDE %s < %t/Utility.ll

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-module -module-name Library \
// RUN:   -parse-as-library \
// RUN:   -o %t/Library.swiftmodule \
// RUN:   %t/Library.swift

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -module-name Library \
// RUN:   -parse-as-library \
// RUN:   -o %t/Library.ll \
// RUN:   %t/Library.swift
// RUN: %FileCheck --check-prefix=SWIFT-LIBRARY-SIDE %s < %t/Library.ll

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -module-name Client \
// RUN:   -parse-as-library \
// RUN:   -I %t \
// RUN:   %t/Client.swift | %FileCheck --check-prefix=SWIFT-CLIENT-SIDE %s

//--- Utility.h
typedef struct {
  int count;
  double value;
} TrivialHiddenCStruct;

// char + long long forces 8-byte alignment the client must honor.
typedef struct {
  char tag;
  long long value;
} PaddedAlignedHiddenCStruct;

TrivialHiddenCStruct makeTrivialHiddenCStruct(int count, double value);
double readTrivialHiddenCStruct(TrivialHiddenCStruct value);
PaddedAlignedHiddenCStruct makePaddedAlignedHiddenCStruct(char tag,
                                                         long long value);
long long readPaddedAlignedHiddenCStruct(PaddedAlignedHiddenCStruct value);

//--- Utility.c
#include "Utility.h"

TrivialHiddenCStruct makeTrivialHiddenCStruct(int count, double value) {
  return (TrivialHiddenCStruct){ count, value };
}

double readTrivialHiddenCStruct(TrivialHiddenCStruct value) {
  return value.count + value.value;
}

PaddedAlignedHiddenCStruct makePaddedAlignedHiddenCStruct(char tag,
                                                         long long value) {
  return (PaddedAlignedHiddenCStruct){ tag, value };
}

long long readPaddedAlignedHiddenCStruct(PaddedAlignedHiddenCStruct value) {
  return value.tag + value.value;
}

//--- Library.swift
public struct TrivialHiddenCStructWrapper {
  var hiddenField: TrivialHiddenCStruct
  public var visibleField: Int
}

public struct PaddedAlignedHiddenCStructWrapper {
  var hiddenField: PaddedAlignedHiddenCStruct
  public var visibleField: Int
}

public func createTrivialWrapper(_ count: Int32, _ value: Double,
                                 _ visibleField: Int)
    -> TrivialHiddenCStructWrapper {
  return TrivialHiddenCStructWrapper(
    hiddenField: makeTrivialHiddenCStruct(count, value),
    visibleField: visibleField)
}

public func readTrivialWrapper(_ wrapper: TrivialHiddenCStructWrapper)
    -> Double {
  return readTrivialHiddenCStruct(wrapper.hiddenField)
}

public func createPaddedAlignedWrapper(_ tag: Int8, _ value: Int64,
                                       _ visibleField: Int)
    -> PaddedAlignedHiddenCStructWrapper {
  return PaddedAlignedHiddenCStructWrapper(
    hiddenField: makePaddedAlignedHiddenCStruct(tag, value),
    visibleField: visibleField)
}

public func readPaddedAlignedWrapper(
  _ wrapper: PaddedAlignedHiddenCStructWrapper
) -> Int64 {
  return readPaddedAlignedHiddenCStruct(wrapper.hiddenField)
}

//--- Client.swift
import Library

// Check that the signatures of functions manipulating a hidden types match on the clang side
// and swift library side. This makes sure we aren't using some lowering for hidden
// types in the swift library that might be easier to reproduce in the swift client,
// but that does not match the clang side.

// CLANG-SIDE-DAG: define [2 x i64] @makeTrivialHiddenCStruct(i32 {{.*}}, double {{.*}})
// CLANG-SIDE-DAG: define double @readTrivialHiddenCStruct([2 x i64] {{.*}})
// CLANG-SIDE-DAG: define [2 x i64] @makePaddedAlignedHiddenCStruct(i8 {{.*}}, i64 {{.*}})
// CLANG-SIDE-DAG: define i64 @readPaddedAlignedHiddenCStruct([2 x i64] {{.*}})

// SWIFT-LIBRARY-SIDE-DAG: declare [2 x i64] @makeTrivialHiddenCStruct(i32 {{.*}}, double {{.*}})
// SWIFT-LIBRARY-SIDE-DAG: declare double @readTrivialHiddenCStruct([2 x i64])
// SWIFT-LIBRARY-SIDE-DAG: declare [2 x i64] @makePaddedAlignedHiddenCStruct(i8 {{.*}}, i64 {{.*}})
// SWIFT-LIBRARY-SIDE-DAG: declare i64 @readPaddedAlignedHiddenCStruct([2 x i64])

// Check that the signatures of functions manipulating the hidden types match on the swift library
// side and swift client side. This makes sure we are able to produce a compatible lowering from
// only the abstract type layout representation of a hidden type.

// SWIFT-LIBRARY-SIDE-DAG: define swiftcc { i32, double, i64 } @"$s7Library20createTrivialWrapper
// SWIFT-LIBRARY-SIDE-DAG: define swiftcc double @"$s7Library18readTrivialWrapper{{.*}}"(i32 {{.*}}, double {{.*}}, i64 {{.*}})
// SWIFT-LIBRARY-SIDE-DAG: define swiftcc { i8, i64, i64 } @"$s7Library26createPaddedAlignedWrapper
// SWIFT-LIBRARY-SIDE-DAG: define swiftcc i64 @"$s7Library24readPaddedAlignedWrapper{{.*}}"(i8 {{.*}}, i64 {{.*}}, i64 {{.*}})

// SWIFT-CLIENT-SIDE-DAG: declare swiftcc { i32, double, i64 } @"$s7Library20createTrivialWrapper{{.*}}"(i32, double, i64)
// SWIFT-CLIENT-SIDE-DAG: declare swiftcc double @"$s7Library18readTrivialWrapper{{.*}}"(i32, double, i64)
// SWIFT-CLIENT-SIDE-DAG: declare swiftcc { i8, i64, i64 } @"$s7Library26createPaddedAlignedWrapper{{.*}}"(i8, i64, i64)
// SWIFT-CLIENT-SIDE-DAG: declare swiftcc i64 @"$s7Library24readPaddedAlignedWrapper{{.*}}"(i8, i64, i64)

// SWIFT-CLIENT-SIDE-DAG: %T7Library27TrivialHiddenCStructWrapperV = type <{ [16 x i8], %TSi }>
// SWIFT-CLIENT-SIDE-DAG: %T7Library33PaddedAlignedHiddenCStructWrapperV = type <{ [16 x i8], %TSi }>

// SWIFT-CLIENT-SIDE-DAG: define {{.*}} @"$s6Client3useyS2iF"
// SWIFT-CLIENT-SIDE-DAG: call void @llvm.memset.{{.*}}(ptr align 8 {{[^,]+}}, i8 0, i64 24, i1 false)
public func use(_ value: Int) -> Int {
  let trivial = createTrivialWrapper(Int32(value), 2.5, value)
  var trivialCopy = trivial
  trivialCopy = trivial

  let aligned = createPaddedAlignedWrapper(1, 42, value)
  var alignedCopy = aligned
  alignedCopy = aligned

  _ = trivialCopy
  _ = alignedCopy
  return MemoryLayout<TrivialHiddenCStructWrapper>.size +
         MemoryLayout<PaddedAlignedHiddenCStructWrapper>.size +
         Int(readTrivialWrapper(trivialCopy)) +
         Int(readPaddedAlignedWrapper(alignedCopy))
}

// Client IR must not reference the bridging header or hidden C functions.
// SWIFT-CLIENT-SIDE-NOT: Utility.h
// SWIFT-CLIENT-SIDE-NOT: @makeTrivialHiddenCStruct
// SWIFT-CLIENT-SIDE-NOT: @readTrivialHiddenCStruct
// SWIFT-CLIENT-SIDE-NOT: @makePaddedAlignedHiddenCStruct
// SWIFT-CLIENT-SIDE-NOT: @readPaddedAlignedHiddenCStruct
