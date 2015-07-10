// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print=BoolBridgingTests -function-definitions=false -print-regular-comments -F %S/Inputs/mock-sdk > %t.txt
// RUN: diff -u <(tail +9 %s) %t.txt

// REQUIRES: objc_interop

// EXPECTED OUTPUT STARTS BELOW THIS LINE.
@exported import Foundation

// stdbool.h uses #define, so this test does as well.

func testCBool(_: Bool) -> Bool
func testObjCBool(_: Bool) -> Bool
func testDarwinBoolean(_: Bool) -> Bool

typealias CBoolTypedef = Bool
typealias ObjCBoolTypedef = ObjCBool
typealias DarwinBooleanTypedef = DarwinBoolean

func testCBoolTypedef(_: CBoolTypedef) -> CBoolTypedef
func testObjCBoolTypedef(_: Bool) -> Bool
func testDarwinBooleanTypedef(_: Bool) -> Bool

func testCBoolPointer(_: UnsafeMutablePointer<Bool>) -> UnsafePointer<Bool>
func testObjCBoolPointer(_: UnsafeMutablePointer<ObjCBool>) -> UnsafePointer<ObjCBool>
func testDarwinBooleanPointer(_: UnsafeMutablePointer<DarwinBoolean>) -> UnsafePointer<DarwinBoolean>

typealias CBoolFn = @convention(c) (Bool) -> Bool
typealias ObjCBoolFn = @convention(c) (ObjCBool) -> ObjCBool
typealias DarwinBooleanFn = @convention(c) (DarwinBoolean) -> DarwinBoolean

typealias CBoolBlock = @convention(block) (Bool) -> Bool
typealias ObjCBoolBlock = @convention(block) (ObjCBool) -> ObjCBool
typealias DarwinBooleanBlock = @convention(block) (DarwinBoolean) -> DarwinBoolean

func testCBoolFnToBlock(_: @convention(c) (Bool) -> Bool) -> (Bool) -> Bool
func testObjCBoolFnToBlock(_: @convention(c) (ObjCBool) -> ObjCBool) -> (Bool) -> Bool
func testDarwinBooleanFnToBlock(_: @convention(c) (DarwinBoolean) -> DarwinBoolean) -> (Bool) -> Bool

class Test : NSObject {
  var propCBool: Bool
  var propObjCBool: Bool
  var propDarwinBoolean: Bool
  
  func testCBool(b: Bool) -> Bool
  func testObjCBool(b: Bool) -> Bool
  func testDarwinBoolean(b: Bool) -> Bool
  
  var propCBoolBlock: (Bool) -> Bool
  var propObjCBoolBlock: (Bool) -> Bool
  var propDarwinBooleanBlock: (Bool) -> Bool
  
  func testCBoolFnToBlock(fp: @convention(c) (Bool) -> Bool) -> (Bool) -> Bool
  func testObjCBoolFnToBlock(fp: @convention(c) (ObjCBool) -> ObjCBool) -> (Bool) -> Bool
  func testDarwinBooleanFnToBlock(fp: @convention(c) (DarwinBoolean) -> DarwinBoolean) -> (Bool) -> Bool
  
  init()
}
