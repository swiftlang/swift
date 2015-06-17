// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

import Foundation

func expectType<T>(_: T.Type, inout _ x: T) {}

func test_NSCoder_decodeObject(coder: NSCoder) {
  var r = coder.decodeObject()
  expectType(Optional<AnyObject>.self, &r)
}

@available(iOS, introduced=9.0)
@available(OSX, introduced=10.11)
func test_NSCoder_decodeTopLevelObject(coder: NSCoder) throws {
  var r = try coder.decodeTopLevelObject()
  expectType(Optional<AnyObject>.self, &r)
}

func test_NSCoder_decodeObjectForKey(coder: NSCoder, key: String) {
  var r = coder.decodeObjectForKey(key)
  expectType(Optional<AnyObject>.self, &r)
}

/*
It is not possible to call this function.

@available(iOS, introduced=9.0)
@available(OSX, introduced=10.11)
func test_NSCoder_decodeObjectForKey_error(coder: NSCoder, key: String) throws {
  var r = try coder.decodeObjectForKey(key)
  expectType(Optional<AnyObject>.self, &r)
}
*/

func test_NSCoder_decodeObjectOfClass_forKey(
  coder: NSCoder, clazz: AnyClass, key: String
) {
  var r = coder.decodeObjectOfClass(clazz, forKey: key)
  expectType(Optional<AnyObject>.self, &r)
}

@available(iOS, introduced=9.0)
@available(OSX, introduced=10.11)
func test_NSCoder_decodeObjectOfClass_forKey_error(
  coder: NSCoder, clazz: AnyClass, key: String
) throws {
  var r = coder.decodeObjectOfClass(clazz, forKey: key)
  expectType(Optional<AnyObject>.self, &r)
}

/*
It wasn't possible to call these APIs from Swift.

func test_NSCoder_decodeObjectOfClasses_forKey(
  coder: NSCoder, classes: NSSet?, key: String
) {
  var r = coder.decodeObjectOfClasses(classes, forKey: key)
  expectType(Optional<AnyObject>.self, &r)
}

@available(iOS, introduced=9.0)
@available(OSX, introduced=10.11)
func test_NSCoder_decodeObjectOfClasses_forKey_error(
  coder: NSCoder, classes: NSSet?, key: String
) {
  var error: NSError?
  var r = coder.decodeObjectOfClasses(classes, forKey: key, error: &error)
  expectType(Optional<AnyObject>.self, &r)
}
*/

func test_NSKeyedUnarchiver_unarchiveObjectWithData(data: NSData) {
  var r = NSKeyedUnarchiver.unarchiveObjectWithData(data)
  expectType(Optional<AnyObject>.self, &r)
}

/*
The API is unavailable and it is not possible to overload on 'throws'.

@available(iOS, introduced=9.0)
@available(OSX, introduced=10.11)
func test_NSKeyedUnarchiver_unarchiveObjectWithData_error(data: NSData) throws {
  var r = NSKeyedUnarchiver.unarchiveObjectWithData(data)
  expectType(Optional<AnyObject>.self, &r)
}
*/

func test_NSKeyedUnarchiver_decodeObjectForKey(
  archiver: NSKeyedUnarchiver, key: String
) {
  var r = archiver.decodeObjectForKey(key)
  expectType(Optional<AnyObject>.self, &r)
}

