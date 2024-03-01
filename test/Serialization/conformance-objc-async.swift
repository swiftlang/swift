// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-emit-module-interface(%t/Conformance.swiftinterface) -module-name Conformance -I %t %t/Conformance.swift
// RUN: %target-swift-frontend -compile-module-from-interface %t/Conformance.swiftinterface -module-name Conformance -o /dev/null -I %t
// REQUIRES: objc_interop

// REQUIRES: rdar119435253

//--- module.modulemap
module ObjCProto {
  header "objc_proto.h"
  export *
}

//--- objc_proto.h
@protocol Doable
- (void)doItWithCompletion:(void (^)())completion;
@end

//--- Conformance.swift
import ObjCProto

public final class ConformsToDoableWithCompletionHandler: Doable {
  public func doIt(completion: @escaping () -> Void) {}
}

@available(SwiftStdlib 5.5, *)
public final class ConformsToDoableWithAsync:Doable {
  public func doIt() async {}
}
