// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

import Foundation

func expectType<T>(_: T.Type, _ x: inout T) {}

func test_NSCoder_decodeObject(_ coder: NSCoder) {
  var r = coder.decodeObject()
  expectType(Optional<AnyObject>.self, &r)
}

@available(iOS, introduced: 9.0)
@available(OSX, introduced: 10.11)
func test_NSCoder_decodeTopLevelObject(_ coder: NSCoder) throws {
  var r = try coder.decodeTopLevelObject()
  expectType(Optional<AnyObject>.self, &r)
}

func test_NSCoder_decodeObjectForKey(_ coder: NSCoder, key: String) {
  var r = coder.decodeObject(forKey: key)
  expectType(Optional<AnyObject>.self, &r)
}

func test_NSCoder_decodeObjectOfClasses_forKey(
  _ coder: NSCoder, classes: NSSet?, key: String
) {
  var r = coder.decodeObjectOfClasses(classes, forKey: key)
  expectType(Optional<AnyObject>.self, &r)
}

@available(iOS, introduced: 9.0)
@available(OSX, introduced: 10.11)
func test_NSCoder_decodeTopLevelObjectOfClasses_forKey_error(
  _ coder: NSCoder, classes: NSSet?, key: String
) throws {
  var r = try coder.decodeTopLevelObjectOfClasses(classes, forKey: key)
  expectType(Optional<AnyObject>.self, &r)
}


func test_NSKeyedUnarchiver_unarchiveObjectWithData(_ data: NSData) {
  var r = NSKeyedUnarchiver.unarchiveObject(with: data)
  expectType(Optional<AnyObject>.self, &r)
}

/*
The API is unavailable and it is not possible to overload on 'throws'.

@available(iOS, introduced: 9.0)
@available(OSX, introduced: 10.11)
func test_NSKeyedUnarchiver_unarchiveObjectWithData_error(_ data: NSData) throws {
  var r = NSKeyedUnarchiver.unarchiveObjectWithData(data)
  expectType(Optional<AnyObject>.self, &r)
}
*/

func test_NSKeyedUnarchiver_decodeObjectForKey(
  _ archiver: NSKeyedUnarchiver, key: String
) {
  var r = archiver.decodeObject(forKey: key)
  expectType(Optional<AnyObject>.self, &r)
}

