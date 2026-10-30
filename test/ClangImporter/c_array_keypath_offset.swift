// Make sure that `MemoryLayout.offset(of:)` returns an offset for both
// C array projections.

// REQUIRES: executable_test
// REQUIRES: swift_feature_ModernImportedCArrays
// UNSUPPORTED: back_deployment_runtime || use_os_stdlib

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift -emit-executable %t/main.swift -o %t/legacy -I %t/include -module-cache-path %t/module-cache -target %target-has-inline-array-triple
// RUN: %target-codesign %t/legacy
// RUN: %target-run %t/legacy | %FileCheck %s

// RUN: %target-build-swift -emit-executable %t/main.swift -o %t/modern -I %t/include -module-cache-path %t/module-cache -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple
// RUN: %target-codesign %t/modern
// RUN: %target-run %t/modern | %FileCheck %s

// Re-run with the optimizer to cover KeyPathProjector.

// RUN: %target-build-swift -O -emit-executable %t/main.swift -o %t/legacy-opt -I %t/include -module-cache-path %t/module-cache -target %target-has-inline-array-triple
// RUN: %target-codesign %t/legacy-opt
// RUN: %target-run %t/legacy-opt | %FileCheck %s

// RUN: %target-build-swift -O -emit-executable %t/main.swift -o %t/modern-opt -I %t/include -module-cache-path %t/module-cache -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple
// RUN: %target-codesign %t/modern-opt
// RUN: %target-run %t/modern-opt | %FileCheck %s

// CHECK: tag 0
// CHECK-NEXT: name 4
// CHECK-NEXT: uuid 20
// CHECK-NEXT: tail 36
// CHECK-NEXT: nested.bytes 8
// CHECK-NEXT: read 7
// CHECK-NEXT: write 7

//--- include/module.modulemap
module CLib {
  header "CLib.h"
  export *
}

//--- include/CLib.h
struct Header {
  int tag;
  char name[16];
  unsigned char uuid[16];
  int tail;
};

struct Outer {
  int pad[2];
  struct { unsigned char bytes[4]; } nested;
};

//--- main.swift
import CLib

func show(_ label: String, _ offset: Int?) {
  print(label, offset.map(String.init) ?? "nil")
}

show("tag", MemoryLayout<Header>.offset(of: \.tag))
show("name", MemoryLayout<Header>.offset(of: \.name))
show("uuid", MemoryLayout<Header>.offset(of: \.uuid))
show("tail", MemoryLayout<Header>.offset(of: \.tail))
show("nested.bytes", MemoryLayout<Outer>.offset(of: \.nested.bytes))

// Reads and writes through the key path see the same bytes as the field.
@inline(never)
func readUUID(_ h: Header) -> UInt8 {
  withUnsafeBytes(of: h[keyPath: \.uuid]) { $0[0] }
}

@inline(never)
func copyUUID(from h: Header, to g: inout Header) {
  g[keyPath: \.uuid] = h[keyPath: \.uuid]
}

var h = Header()
withUnsafeMutableBytes(of: &h) { $0[20] = 7 }
print("read", readUUID(h))

var g = Header()
copyUUID(from: h, to: &g)
print("write", withUnsafeBytes(of: &g) { $0[20] })
