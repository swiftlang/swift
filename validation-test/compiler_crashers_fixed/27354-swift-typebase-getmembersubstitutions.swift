// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class a<T where B:B{
enum B{class b<c{{}func g
class B:b{func g
