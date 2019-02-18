//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Intents
import Foundation

#if os(iOS) || os(watchOS)

public protocol _INIntentSetImageKeyPath { }

extension _INIntentSetImageKeyPath {
    
    @available(iOS 12.0, watchOS 5.0, *)
    public func setImage<Value>(_ image: INImage?, forParameterNamed parameterName: KeyPath<Self, Value>) {
        if let keyPathString = parameterName._kvcKeyPathString {
            (self as! INIntent).__setImage(image, forParameterNamed: keyPathString)
        }
    }
    
    @available(iOS 12.0, watchOS 5.0, *)
    public func image<Value>(forParameterNamed parameterName: KeyPath<Self, Value>) -> INImage? {
        if let keyPathString = parameterName._kvcKeyPathString {
            return (self as! INIntent).__image(forParameterNamed: keyPathString)
        } else {
            return nil
        }
    }
}

@available(iOS 10.0, watchOS 3.2, *)
extension INIntent : _INIntentSetImageKeyPath { }

#endif
