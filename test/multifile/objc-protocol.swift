// RUN: %target-swift-frontend -module-name test -emit-ir -primary-file %s %S/Inputs/objc-protocol-other.swift -import-objc-header %S/Inputs/objc-protocol-bridging.h -sdk %sdk -o %t.o

// REQUIRES: objc_interop

import Foundation

@objc class Foo : NSObject, ObjCProtocol { }
