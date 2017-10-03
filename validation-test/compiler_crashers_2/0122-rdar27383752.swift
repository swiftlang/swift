// RUN: not --crash %target-swift-frontend %s -emit-ir
// REQUIRES: OS=macosx

import Foundation
import CoreData

// Does not segfault

@available(macOS 10.12, *)
class Foo<T>: NSObject, NSFetchedResultsControllerDelegate {
    override init() {
        super.init()
    }
    
    @nonobjc func controllerWillChangeContent(_ controller: NSFetchedResultsController<NSFetchRequestResult>) {
    }
}

// Segfaults

@available(macOS 10.12, *)
class Bar<T>: NSObject {
    override init() {
        super.init()
    }
}

@available(macOS 10.12, *)
extension Bar: NSFetchedResultsControllerDelegate {
    @nonobjc func controllerWillChangeContent(_ controller: NSFetchedResultsController<NSFetchRequestResult>) {
    }
}
