// RUN: %target-build-swift -g %s -emit-ir | %FileCheck %s

// https://github.com/apple/swift/issues/66554
// IRGenDebugInfo type reconstruction crash because existential types
// inside typealias are not taken into account when comparing type
// equality

protocol Protocol<T> { associatedtype T }
typealias AnyProtocol<T> = any Protocol<T>
let crash: AnyProtocol<Any?>

// CHECK: @"$s4main5crashAA8Protocol_pypSg1TAaCPRts_XPvp" =
// CHECK: !DIGlobalVariable(name: "crash", linkageName: "$s4main5crashAA8Protocol_pypSg1TAaCPRts_XPvp"
