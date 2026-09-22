// RUN: %target-swift-emit-ir -parse-as-library -module-name main -verify %s -enable-experimental-feature Embedded -wmo

// REQUIRES: swift_feature_Embedded

// A class defined in the same module as the cast has a unique definition (the
// main module's metadata), so a same-module downcast is fine and must not be
// diagnosed as a cross-module non-unique cast. rdar://179424428

public protocol WriterProtocol: AnyObject {}
public protocol ResourceWriter: WriterProtocol {}

public final class PSWriterM3Demo: ResourceWriter {
  public init() {}
}

@inline(never)
public func makeWriter() -> any ResourceWriter { PSWriterM3Demo() }

public func sameModuleCast() -> Bool {
  let w = makeWriter()
  return (w as? PSWriterM3Demo) != nil // no error: same-module class has a unique definition
}
