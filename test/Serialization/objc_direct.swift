// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

/// Round-trip @objcDirect through a *binary* .swiftmodule.
///
/// test/ModuleInterface/objc_direct.swift covers the textual .swiftinterface
/// path, which is a different mechanism: there the attribute is printed and
/// re-parsed, whereas here it has to survive serialization and deserialization
/// by record code. A serialization code is only observable through a
/// round-trip, so without this test breaking it outright would produce no
/// signal at all.
///
/// The oracle is witness_objc_direct. That diagnostic can only be emitted if
/// the *deserialized* method still carries the attribute, so its presence
/// proves the round-trip rather than merely proving the module loaded. The
/// conformance is deliberately declared in the client, not the library, so the
/// witness being checked is the imported decl.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Library with the attribute, emitted as a binary .swiftmodule.
// RUN: %target-swift-frontend -emit-module -o %t/DirectLib.swiftmodule \
// RUN:   -module-name DirectLib -enable-experimental-feature ObjCDirect \
// RUN:   %t/DirectLib.swift

/// The attribute survived: conforming the imported class in the client is
/// rejected.
// RUN: %target-swift-frontend -typecheck -verify -I %t \
// RUN:   -verify-additional-prefix direct- \
// RUN:   -enable-experimental-feature ObjCDirect %t/Client.swift

/// Negative control. Rebuild the same library with the method as plain @objc
/// and check the error is *absent*, which is what shows the diagnostic tracks
/// the attribute rather than something incidental to importing the class.
///
/// Note the substitution replaces @objcDirect with @objc rather than deleting
/// it. @objcDirect implies @objc, so deleting it would also remove the @objc
/// and the control would fail with "does not satisfy requirement" -- failing
/// for an unrelated reason, i.e. controlling for the wrong variable.
// RUN: %empty-directory(%t/plain)
// RUN: sed 's/@objcDirect/@objc/' %t/DirectLib.swift > %t/plain/DirectLib.swift
// RUN: %target-swift-frontend -emit-module -o %t/plain/DirectLib.swiftmodule \
// RUN:   -module-name DirectLib %t/plain/DirectLib.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t/plain \
// RUN:   -enable-experimental-feature ObjCDirect %t/Client.swift

//--- DirectLib.swift

import Foundation

@objc public protocol Greeter {
  func greet()
}

@objc public class DirectGreeter: NSObject {
  @objcDirect public final func greet() {}
}

//--- Client.swift

import DirectLib

/// The witness is the deserialized greet() from DirectLib, not a declaration in
/// this file. Under the control build the same line is accepted.
extension DirectGreeter: Greeter {}
// expected-direct-error@-1 {{cannot satisfy a requirement of '@objc' protocol 'Greeter' because it is a direct method}}
