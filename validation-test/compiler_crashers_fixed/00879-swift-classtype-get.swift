// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
import CoreData
class A : NSManagedObject {
func b<T: q o f.g == g> {
}
protocol q {
}
func q<k:q
