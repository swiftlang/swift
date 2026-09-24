// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Marking the class @export(interface) gives it a unique metadata definition,
// so the cross-module downcast is sound and must not be diagnosed.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -wmo -parse-as-library -c -I %t %t/Lib.swift -o %t/Lib.o -emit-module -emit-module-path %t/Lib.swiftmodule -emit-empty-object-file
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -wmo -parse-as-library -emit-ir -verify -I %t %t/Main.swift

// REQUIRES: swift_feature_Embedded

// rdar://179424428

//--- Lib.swift

public protocol WriterProtocol: AnyObject {}
public protocol ResourceWriter: WriterProtocol {}

@export(interface)
public final class PSWriterM3Demo: ResourceWriter {
  public init() {}
}

@inline(never)
public func makeWriter() -> any ResourceWriter { PSWriterM3Demo() }

//--- Main.swift

import Lib

public func conditionalCast() -> Bool {
  let w = makeWriter()
  return (w as? PSWriterM3Demo) != nil // no error: @export(interface) makes the metadata unique
}
