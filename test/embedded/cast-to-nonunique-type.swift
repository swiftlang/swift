// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The defining module builds fine; the cast lives in the consuming module.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -wmo -parse-as-library -c -I %t %t/Lib.swift -o %t/Lib.o -emit-module -emit-module-path %t/Lib.swiftmodule -emit-empty-object-file
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -wmo -parse-as-library -emit-ir -verify -I %t %t/Main.swift

// REQUIRES: swift_feature_Embedded

// A class-bound existential downcast to a concrete class defined in another
// module is unsound in Embedded Swift: the class's type metadata is emitted
// per-module (linkonce_odr) unless it is @export(interface), so the metadata
// record the cast compares against may differ from the one the allocating
// module stamped into the object. Diagnose it. rdar://179424428

//--- Lib.swift

public protocol WriterProtocol: AnyObject {}
public protocol ResourceWriter: WriterProtocol {}

public final class PSWriterM3Demo: ResourceWriter {
  public init() {}
}

//--- Main.swift

import Lib

// The writer is passed in as an existential so its dynamic type is opaque and
// the cast cannot be folded away.

public func conditionalCast(_ w: any ResourceWriter) -> PSWriterM3Demo? {
  return w as? PSWriterM3Demo // expected-warning {{casting to 'PSWriterM3Demo' across a module boundary in embedded Swift may fail at runtime because its type metadata is not unique; mark 'PSWriterM3Demo' with '@export(interface)' in its defining module}}
}

public func forcedCast(_ w: any ResourceWriter) -> PSWriterM3Demo {
  return w as! PSWriterM3Demo // expected-warning {{casting to 'PSWriterM3Demo' across a module boundary in embedded Swift may fail at runtime because its type metadata is not unique; mark 'PSWriterM3Demo' with '@export(interface)' in its defining module}}
}
