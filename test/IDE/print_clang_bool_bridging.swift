// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print=BoolBridgingTests -function-definitions=false -print-regular-comments -F %S/Inputs/mock-sdk > %t.txt
// RUN: diff -u <(tail +9 %s) %t.txt

// REQUIRES: objc_interop

// EXPECTED OUTPUT STARTS BELOW THIS LINE.
@_exported import Foundation

// stdbool.h uses #define, so this test does as well.

@discardableResult
func testCBool(_: Bool) -> Bool
@discardableResult
func testObjCBool(_: Bool) -> Bool
@discardableResult
func testDarwinBoolean(_: Bool) -> Bool

typealias CBoolTypedef = Bool
typealias ObjCBoolTypedef = ObjCBool
typealias DarwinBooleanTypedef = DarwinBoolean

@discardableResult
func testCBoolTypedef(_: CBoolTypedef) -> CBoolTypedef
@discardableResult
func testObjCBoolTypedef(_: Bool) -> Bool
@discardableResult
func testDarwinBooleanTypedef(_: Bool) -> Bool

@discardableResult
func testCBoolPointer(_: UnsafeMutablePointer<Bool>) -> UnsafePointer<Bool>
@discardableResult
func testObjCBoolPointer(_: UnsafeMutablePointer<ObjCBool>) -> UnsafePointer<ObjCBool>
@discardableResult
func testDarwinBooleanPointer(_: UnsafeMutablePointer<DarwinBoolean>) -> UnsafePointer<DarwinBoolean>

typealias CBoolFn = @convention(c) (Bool) -> Bool
typealias ObjCBoolFn = @convention(c) (ObjCBool) -> ObjCBool
typealias DarwinBooleanFn = @convention(c) (DarwinBoolean) -> DarwinBoolean

typealias CBoolBlock = (Bool) -> Bool
typealias ObjCBoolBlock = (Bool) -> Bool
typealias DarwinBooleanBlock = (Bool) -> Bool

@discardableResult
func testCBoolFnToBlock(_: @convention(c) (Bool) -> Bool) -> (Bool) -> Bool
@discardableResult
func testObjCBoolFnToBlock(_: @convention(c) (ObjCBool) -> ObjCBool) -> (Bool) -> Bool
@discardableResult
func testDarwinBooleanFnToBlock(_: @convention(c) (DarwinBoolean) -> DarwinBoolean) -> (Bool) -> Bool

@discardableResult
func testCBoolFnToBlockTypedef(_: CBoolFn) -> CBoolBlock
@discardableResult
func testObjCBoolFnToBlockTypedef(_: ObjCBoolFn) -> ObjCBoolBlock
@discardableResult
func testDarwinBooleanFnToBlockTypedef(_: DarwinBooleanFn) -> DarwinBooleanBlock

typealias CBoolFnToBlockType = (CBoolFn) -> CBoolBlock
typealias ObjCCBoolFnToBlockType = (ObjCBoolFn) -> (ObjCBool) -> ObjCBool
typealias DarwinBooleanFnToBlockType = (DarwinBooleanFn) -> (DarwinBoolean) -> DarwinBoolean

var globalCBoolFn: CBoolFn
var globalObjCBoolFn: ObjCBoolFn
var globalDarwinBooleanFn: DarwinBooleanFn

var globalCBoolBlock: @convention(block) (Bool) -> Bool
var globalObjCBoolBlock: @convention(block) (ObjCBool) -> ObjCBool
var globalDarwinBooleanBlock: @convention(block) (DarwinBoolean) -> DarwinBoolean

class Test : NSObject {
  var propCBool: Bool
  var propObjCBool: Bool
  var propDarwinBoolean: Bool
  
  @discardableResult
  func testCBool(_ b: Bool) -> Bool
  @discardableResult
  func testObjCBool(_ b: Bool) -> Bool
  @discardableResult
  func testDarwinBoolean(_ b: Bool) -> Bool
  
  var propCBoolBlock: (Bool) -> Bool
  var propObjCBoolBlock: (Bool) -> Bool
  var propDarwinBooleanBlock: (Bool) -> Bool
  
  @discardableResult
  func testCBoolFn(toBlock fp: @convention(c) (Bool) -> Bool) -> (Bool) -> Bool
  @discardableResult
  func testObjCBoolFn(toBlock fp: @convention(c) (ObjCBool) -> ObjCBool) -> (Bool) -> Bool
  @discardableResult
  func testDarwinBooleanFn(toBlock fp: @convention(c) (DarwinBoolean) -> DarwinBoolean) -> (Bool) -> Bool
  
  func produceCBoolBlockTypedef(_ outBlock: AutoreleasingUnsafeMutablePointer<(@convention(block) (Bool) -> Bool)?>)
  func produceObjCBoolBlockTypedef(_ outBlock: AutoreleasingUnsafeMutablePointer<(@convention(block) (ObjCBool) -> ObjCBool)?>)
  func produceDarwinBooleanBlockTypedef(_ outBlock: AutoreleasingUnsafeMutablePointer<(@convention(block) (DarwinBoolean) -> DarwinBoolean)?>)
  
  init()
}
