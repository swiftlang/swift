// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -swift-version 5 %t/OtherLibrary.swift -emit-module -module-name OtherLibrary -o %t
// RUN: %target-swift-frontend -typecheck %t/Library.swift -module-name Library -verify -swift-version 5 -import-underlying-module -I %t
// RUN: %target-swift-frontend -typecheck %t/Library.swift -module-name Library -verify -swift-version 5 -DEXPLICIT_IMPORT -I %t

// REQUIRES: objc_interop

//--- module.modulemap

module Library {
  header "Library.h"
}

//--- Library.h

@import Foundation;

@interface UnderlyingLibraryClass : NSObject
@end

@protocol UnderlyingLibraryProtocol
@end

//--- OtherLibrary.swift

public class OtherLibraryClass {}
public protocol OtherLibraryProtocol {}

//--- Library.swift

#if EXPLICIT_IMPORT
@_exported import Library
#endif
import OtherLibrary

public class LibraryClass {}
public protocol LibraryProtocol {}

extension LibraryClass: UnderlyingLibraryProtocol {}
extension LibraryClass: LibraryProtocol {}
extension LibraryClass: OtherLibraryProtocol {}
extension UnderlyingLibraryClass: OtherLibraryProtocol {}
extension UnderlyingLibraryClass: LibraryProtocol {}
extension UnderlyingLibraryClass: UnderlyingLibraryProtocol {}
extension OtherLibraryClass: UnderlyingLibraryProtocol {}
extension OtherLibraryClass: LibraryProtocol {}
extension OtherLibraryClass: OtherLibraryProtocol {} // expected-warning {{extension declares a conformance of imported type 'OtherLibraryClass' to imported protocol 'OtherLibraryProtocol'}}
// expected-note @-1 {{add '@retroactive' to silence this warning}} {{30-50=@retroactive OtherLibraryProtocol}}
