// XFAIL: linux
// RUN: rm -rf %t && mkdir %t
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -parse -primary-file %t/main.swift -emit-reference-dependencies-path - > %t.swiftdeps

// SR-1267, SR-1270
import Foundation

class TypeType<T where T: NSString> {
    func call(notification: NSNotification?) {
        let set = NSOrderedSet()
        if let objects = set.array as? [T] {
            let _ = (notification?.userInfo?["great_key"] as? NSSet).flatMap { updatedObjects in
                return updatedObjects.filter({ element in
                    guard let element = element as? T
                        where objects.indexOf(element) != nil
                    else {
                        return false
                    }
                    return true
                })
            } ?? []
        }
    }
}