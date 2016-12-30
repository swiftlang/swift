// RUN: %target-swift-frontend %s -emit-ir
// RUN: %target-swift-frontend %s -emit-ir -O

// REQUIRES: objc_interop

import Foundation

/// This function returns true if inspectedClass is subclass of wantedSuperclass.
/// This is achieved by climbing the class tree hierarchy using class_getSuperclass
/// runtime function. This is useful when examining classes that do not have
/// NSObject as root (e.g. NSProxy subclasses).
private func XUClassKindOfClass(_ inspectedClass: AnyClass?, wantedSuperclass: AnyClass?) -> Bool {
	// We've hit the root, so no, it's not
	if inspectedClass == nil {
		return false
	}

	// It's the class, yay!
	if inspectedClass == wantedSuperclass {
		return true
	}

	// Recursively call the function on the superclass of inspectedClass
	return XUClassKindOfClass(class_getSuperclass(inspectedClass), wantedSuperclass: wantedSuperclass)
}

/// Works pretty much as +isKindOfClass: on NSObject, but will work fine even with
/// NSProxy subclasses, which do not respond to +isKindOfClass:
public func XUClassIsSubclassOfClass(_ superclass: AnyClass, subclass: AnyClass) -> Bool {
	return XUClassKindOfClass(subclass, wantedSuperclass: superclass)
}

/// Returns a list of subclasses of class T. Doesn't include the root T class.
public func XUAllSubclassesOfClass<T: AnyObject>(_ aClass: T.Type) -> [T.Type] {
	var result: [T.Type] = []

	var numClasses: Int32 = 0

	// Get the number of classes in the ObjC runtime
	numClasses = objc_getClassList(nil, 0)

	if numClasses > 0 {
		// Get them all
		let memory = malloc(MemoryLayout<AnyClass>.size * Int(numClasses))!
		defer {
			free(memory)
		}
		
		let classesPtr = memory.assumingMemoryBound(to: Optional<AnyClass>.self)
		let classes = AutoreleasingUnsafeMutablePointer<AnyClass?>(classesPtr)
		numClasses = objc_getClassList(classes, numClasses)

		for i in 0 ..< Int(numClasses) {
			// Go through the classes, find out if the class is kind of aClass
			// and then add it to the list

			guard let cl = classes[i] else {
				continue
			}

			if XUClassKindOfClass(cl, wantedSuperclass: aClass) && cl != aClass {
				result.append(cl as! T.Type)
			}
		}
	}

	return result
}
