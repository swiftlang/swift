// RUN: %target-swift-frontend %s -typecheck -verify
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
    // expected-error@-1 {{conformance of generic class 'Bar<T>' to @objc protocol 'NSFetchedResultsControllerDelegate' cannot be in an extension}}
    @nonobjc func controllerWillChangeContent(_ controller: NSFetchedResultsController<NSFetchRequestResult>) {
    }
}
