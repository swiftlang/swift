// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class B<f{func a{struct B<w{class d protocol c{func a<T:f.a
