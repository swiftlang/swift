// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B<T{func g<f{let A{protocol A{{}func a(f.c}}var d=(g<T>(f<T
