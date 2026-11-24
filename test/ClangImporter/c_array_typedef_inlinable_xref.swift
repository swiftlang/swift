// Make sure that declarations with dual C array projections have distinct
// signatures during deserialization even when the C array is in a typedef.
// Reproducer for rdar://190152857.

// REQUIRES: swift_feature_ModernImportedCArrays

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Library built from its swiftinterface, client using the same projection
// RUN: %empty-directory(%t/legacy)
// RUN: %target-swift-frontend -emit-module -emit-module-interface-path %t/legacy/Lib.swiftinterface -module-name Lib -enable-library-evolution -parse-as-library %t/Lib.swift -I %t/include -o %t/legacy/Lib.swiftmodule -target %target-has-inline-array-triple
// RUN: %target-swift-frontend -c -O -primary-file %t/Client.swift -module-name Client -I %t/legacy -I %t/include -o /dev/null -target %target-has-inline-array-triple

// RUN: %empty-directory(%t/modern)
// RUN: %target-swift-frontend -emit-module -emit-module-interface-path %t/modern/Lib.swiftinterface -module-name Lib -enable-library-evolution -parse-as-library %t/Lib.swift -I %t/include -o %t/modern/Lib.swiftmodule -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple
// RUN: %target-swift-frontend -c -O -primary-file %t/Client.swift -module-name Client -I %t/modern -I %t/include -o /dev/null -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple

//--- include/module.modulemap
module CLib {
  header "CLib.h"
  export *
}

//--- include/CLib.h
typedef float Pixel[4];

void lookup(const unsigned char *src, unsigned char *dst, const Pixel table[256]);

//--- Lib.swift
import CLib

@inlinable
public func applyLookup(_ table: [Pixel], _ src: UnsafePointer<UInt8>, _ dst: UnsafeMutablePointer<UInt8>) {
  lookup(src, dst, table)
}

//--- Client.swift
import Lib
import CLib

@inline(never)
func use(_ table: [Pixel], _ src: UnsafePointer<UInt8>, _ dst: UnsafeMutablePointer<UInt8>) {
  applyLookup(table, src, dst)
}
