// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -swift-version 5 %t/Library.swift -emit-module -module-name Library -o %t -package-name Library 
// RUN: %target-swift-frontend -swift-version 5 %t/OtherLibrary.swift -emit-module -module-name OtherLibrary -o %t -package-name Library
// RUN: %target-swift-frontend -swift-version 5 %t/ExternalLibrary.swift -emit-module -module-name ExternalLibrary -o %t
// RUN: %target-swift-frontend -typecheck %t/Client.swift -module-name Client -verify -swift-version 5 -I %t -package-name Library

//--- Library.swift

public class LibraryClass {}
public protocol LibraryProtocol {}
package class PackageLibraryClass {}
package protocol PackageLibraryProtocol {}

//--- OtherLibrary.swift

public class OtherLibraryClass {}
public protocol OtherLibraryProtocol {}
package class PackageOtherLibraryClass {}
package protocol PackageOtherLibraryProtocol {}

//--- ExternalLibrary.swift

public class ExternalLibraryClass {}
public protocol ExternalLibraryProtocol {}

//--- Client.swift

public import Library
public import OtherLibrary
public import ExternalLibrary

// These are all fine because all 3 of these modules are in the same package.

extension LibraryClass: LibraryProtocol {}
extension OtherLibraryClass: LibraryProtocol {}
extension LibraryClass: OtherLibraryProtocol {}

extension PackageLibraryClass: LibraryProtocol {}
extension PackageOtherLibraryClass: LibraryProtocol {}
extension PackageLibraryClass: OtherLibraryProtocol {}

extension LibraryClass: PackageLibraryProtocol {}
extension OtherLibraryClass: PackageLibraryProtocol {}
extension LibraryClass: PackageOtherLibraryProtocol {}

extension PackageLibraryClass: PackageLibraryProtocol {}
extension PackageOtherLibraryClass: PackageLibraryProtocol {}
extension PackageLibraryClass: PackageOtherLibraryProtocol {}

extension ExternalLibraryClass: ExternalLibraryProtocol {} // expected-warning {{extension declares a conformance of imported type 'ExternalLibraryClass' to imported protocol 'ExternalLibraryProtocol'; this will not behave correctly if the owners of 'ExternalLibrary' introduce this conformance in the future}}
// expected-note @-1 {{add '@retroactive' to silence this warning}} {{33-56=@retroactive ExternalLibraryProtocol}}

extension ExternalLibraryClass: LibraryProtocol {}
extension LibraryClass: ExternalLibraryProtocol {}

extension ExternalLibraryClass: @retroactive OtherLibraryProtocol {} // expected-warning {{'retroactive' attribute does not apply; 'OtherLibraryProtocol' is declared in the same package; this is an error in the Swift 6 language mode}}
extension OtherLibraryClass: @retroactive ExternalLibraryProtocol {} // expected-warning {{'retroactive' attribute does not apply; 'OtherLibraryClass' is declared in the same package; this is an error in the Swift 6 language mode}}
