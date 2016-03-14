// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class x{struct D:A}protocol A{typealias d:e{{}}func a<func a:a func a<D:D.f
