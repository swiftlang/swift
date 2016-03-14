// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
import CoreData:let a{struct Q{class B{}}class a{class A{func a<U:U.B
