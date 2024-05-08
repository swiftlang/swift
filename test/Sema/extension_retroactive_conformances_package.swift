// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -swift-version 5 %t/OtherLibrary.swift -package-name Library -emit-module -module-name OtherLibrary -package-name Library -o %t
// RUN: %target-swift-frontend -typecheck %t/Library.swift -module-name Library -package-name Library -verify -swift-version 5 -import-underlying-module -I %t
// RUN: %target-swift-frontend -typecheck %t/Client.swift -module-name Client -package-name Library -verify -swift-version 5 -I %t

//--- Library.swift

public class LibraryClass {
}

public protocol LibraryProtocol {
}

//--- OtherLibrary.swift
public class OtherLibraryClass {}
public class OtherLibraryProtocol {}

//--- Client.swift

import Library
import OtherLibrary

// These are all fine because all 3 of these libraries are in the same package.

extension LibraryClass: LibraryProtocol {}
extension OtherLibraryClass: LibraryProtocol {}
extension LibraryClass: OtherLibraryProtocol {}