// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-ir %s
// REQUIRES: objc_interop
//
// Ensures this does not cause a crash, as @objc protocols are a special case
// because they do not require a witness table
// rdar://problem/59740179
import Foundation

@objc
public protocol MyProtocol {}

extension NSIndexSet: MyProtocol {}

public func toSomeMyProtocol() -> some MyProtocol { 
    return NSIndexSet(index: 7)
}
