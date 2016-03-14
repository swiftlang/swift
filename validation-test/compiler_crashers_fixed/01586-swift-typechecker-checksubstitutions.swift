// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct D : NSObject {
func compose<f : () -> String {
struct S<d where S<D> == F
