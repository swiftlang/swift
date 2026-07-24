// Runtime test: values of encapsulated types round-trip correctly across the
// dylib boundary, proving the layout-table-driven codegen is correct.

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes
// REQUIRES: executable_test
// REQUIRES: PTRSIZE=64

// UNSUPPORTED: remote_run || device_run

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-clang -x c %t/Utility.c -c -o %t/Utility.o

// Build Library as a dylib. -wmo skips the merge-modules step.
// RUN: %target-build-swift \
// RUN:   -Xfrontend -internal-import-bridging-header \
// RUN:   -Xfrontend %t/Utility.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -wmo -parse-as-library -emit-library \
// RUN:   -emit-module -emit-module-path %t/Library.swiftmodule \
// RUN:   -module-name Library \
// RUN:   %t/Library.swift \
// RUN:   %t/Utility.o \
// RUN:   -o %t/%target-library-name(Library)

// Build Client with -Onone to prevent the optimizer from folding away copies.
// RUN: %target-build-swift -Onone \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -I %t -L %t -lLibrary \
// RUN:   %target-rpath(%t) \
// RUN:   -o %t/main %t/Client.swift
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main | %FileCheck %s

//--- Utility.h
typedef struct {
  int count;
  double value;
} TrivialHiddenCStruct;

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
public struct S {
  private var payload: TrivialHiddenCStruct
  private var aligned: PaddedAlignedHiddenCStruct
  public var x: Int
  public init(count: Int32, value: Double, tag: Int8,
              alignedValue: Int64, x: Int) {
    payload = makeTrivialHiddenCStruct(count, value)
    aligned = makePaddedAlignedHiddenCStruct(tag, alignedValue)
    self.x = x
  }
  public func payloadValue() -> Double {
    return readTrivialHiddenCStruct(payload)
  }
  public func alignedValue() -> Int64 {
    return readPaddedAlignedHiddenCStruct(aligned)
  }
}

//--- Client.swift
import Library

@inline(never)
func roundTrip() -> Double {
  let s = S(count: 42, value: 0.5, tag: 1, alignedValue: 41, x: 100)
  var s2 = s
  s2 = S(count: 99, value: 0.5, tag: 2, alignedValue: 98, x: 200)
  s2 = s
  return s2.payloadValue()
}

@inline(never)
func roundTripAllFields() -> (Double, Int64, Int) {
  let s = S(count: 42, value: 0.5, tag: 1, alignedValue: 41, x: 100)
  var s2 = s
  s2 = S(count: 99, value: 0.5, tag: 2, alignedValue: 98, x: 200)
  s2 = s
  return (s2.payloadValue(), s2.alignedValue(), s2.x)
}

@inline(never)
func roundTripOptional() -> Double {
  var opt: S? = S(count: 42, value: 0.5, tag: 1, alignedValue: 41, x: 100)
  opt = nil
  opt = S(count: 77, value: 0.5, tag: 2, alignedValue: 98, x: 200)
  return opt!.payloadValue()
}

// CHECK: size=40 alignment=8 stride=40
print("size=\(MemoryLayout<S>.size) "
    + "alignment=\(MemoryLayout<S>.alignment) "
    + "stride=\(MemoryLayout<S>.stride)")

// CHECK-NEXT: payload=42.5
print("payload=\(roundTrip())")

// CHECK-NEXT: all-fields payload=42.5 aligned=42 x=100
let (payload, aligned, x) = roundTripAllFields()
print("all-fields payload=\(payload) aligned=\(aligned) x=\(x)")

// CHECK-NEXT: optional=77.5
print("optional=\(roundTripOptional())")
