// Test that extra inhabitants in a trivial Clang record are preserved when
// lowering from its serialized hidden representation.

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes
// REQUIRES: executable_test

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -parse-as-library -emit-object \
// RUN:   -emit-module -emit-module-path %t/Library.swiftmodule \
// RUN:   -module-name Library %t/Library.swift \
// RUN:   -o %t/Library.o

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -o %t/Library.ll -module-name Library -parse-as-library \
// RUN:   %t/Library.swift
// RUN: %FileCheck --check-prefix=LIBRARY-SIDE %s < %t/Library.ll

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -o %t/Client.ll -module-name Client \
// RUN:   -I %t %t/Client.swift
// RUN: %FileCheck --check-prefix=CLIENT-SIDE %s < %t/Client.ll

// RUN: %target-build-swift \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -I %t -o %t/client %t/Client.swift %t/Library.o
// RUN: %target-codesign %t/client
// RUN: %target-run %t/client

//--- Utility.h
typedef struct {
  _Bool value;
} BoolHiddenCStruct;

//--- Library.swift
public struct BoolHiddenCStructWrapper {
  var hiddenField: BoolHiddenCStruct

  public init(_ value: Bool) {
    hiddenField = BoolHiddenCStruct(value: value)
  }

  public var value: Bool { hiddenField.value }
}

public func passThroughOptional(_ value: BoolHiddenCStructWrapper?)
    -> BoolHiddenCStructWrapper? {
  value
}

public func readOptional(_ value: BoolHiddenCStructWrapper?) -> Bool? {
  value?.value
}

public func makeOptional(_ value: Bool?) -> BoolHiddenCStructWrapper? {
  value.map(BoolHiddenCStructWrapper.init)
}

public func optionalBoolHiddenCStructWrapperSize() -> Int {
  MemoryLayout<BoolHiddenCStructWrapper?>.size
}

//--- Client.swift
import Library

assert(MemoryLayout<BoolHiddenCStructWrapper>.size == 1)
assert(MemoryLayout<BoolHiddenCStructWrapper?>.size == 1)
assert(optionalBoolHiddenCStructWrapperSize() == 1)

assert(passThroughOptional(nil) == nil)
assert(passThroughOptional(BoolHiddenCStructWrapper(false))?.value == false)
assert(passThroughOptional(BoolHiddenCStructWrapper(true))?.value == true)
assert(readOptional(nil) == nil)
assert(readOptional(BoolHiddenCStructWrapper(false)) == false)
assert(readOptional(BoolHiddenCStructWrapper(true)) == true)
assert(makeOptional(nil) == nil)
assert(makeOptional(false)?.value == false)
assert(makeOptional(true)?.value == true)

// The hidden _Bool field provides extra inhabitants, allowing Optional to use
// the same byte of storage as its payload.
// LIBRARY-SIDE-DAG: define {{.*}}swiftcc i8 @"$s7Library19passThroughOptionalyAA24BoolHiddenCStructWrapperVSgAEF"(i8 {{.*}})
// CLIENT-SIDE-DAG: declare {{.*}}swiftcc i8 @"$s7Library19passThroughOptionalyAA24BoolHiddenCStructWrapperVSgAEF"(i8)
