//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Intents
import Foundation

extension NSString {
    @available(OSX 10.14, iOS 12.0, watchOS 5.0, *)
    public class func deferredLocalizedIntentsString(with format: String, _ args: CVarArg...) -> NSString {
        return withVaList(args) {
            self.__deferredLocalizedIntentsString(withFormat: format, fromTable: nil, arguments: $0) as NSString
        }
    }
    
    @available(OSX 10.14, iOS 12.0, watchOS 5.0, *)
    public class func deferredLocalizedIntentsString(with format: String, table: String, _ args: CVarArg...) -> NSString {
        return withVaList(args) {
            self.__deferredLocalizedIntentsString(withFormat: format, fromTable: table, arguments: $0) as NSString
        }
    }
    
    @available(OSX 10.14, iOS 12.0, watchOS 5.0, *)
    public class func deferredLocalizedIntentsString(with format: String, table: String, arguments: CVaListPointer) -> NSString {
        return self.__deferredLocalizedIntentsString(withFormat: format, fromTable: table, arguments: arguments) as NSString
    }
}
