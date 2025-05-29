// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print=BoolBridgingTests -function-definitions=false -skip-unavailable -F %S/Inputs/mock-sdk > %t.txt
// RUN: diff -u <(tail -n +9 %s) %t.txt

// REQUIRES: objc_interop

// EXPECTED OUTPUT STARTS BELOW THIS LINE.
@_exported import Foundation

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
typealias CBoolBlock = (Bool) -> Bool
typealias ObjCBoolBlock = (Bool) -> Bool
typealias DarwinBooleanBlock = (Bool) -> Bool
func testCBoolFnToBlock(_: @convention(c) (Bool) -> Bool) -> (Bool) -> Bool
func testObjCBoolFnToBlock(_: @convention(c) (ObjCBool) -> ObjCBool) -> (Bool) -> Bool
func testDarwinBooleanFnToBlock(_: @convention(c) (DarwinBoolean) -> DarwinBoolean) -> (Bool) -> Bool
func testCBoolFnToBlockTypedef(_: CBoolFn) -> CBoolBlock
func testObjCBoolFnToBlockTypedef(_: ObjCBoolFn) -> ObjCBoolBlock
func testDarwinBooleanFnToBlockTypedef(_: DarwinBooleanFn) -> DarwinBooleanBlock
typealias CBoolFnToBlockType = (CBoolFn) -> (Bool) -> Bool
typealias ObjCBoolFnToBlockType = (ObjCBoolFn) -> (ObjCBool) -> ObjCBool
typealias DarwinBooleanFnToBlockType = (DarwinBooleanFn) -> (DarwinBoolean) -> DarwinBoolean
var globalObjCBoolFnToBlockFP: @convention(c) (ObjCBoolFn) -> (ObjCBool) -> ObjCBool
var globalObjCBoolFnToBlockFPP: UnsafeMutablePointer<@convention(c) (ObjCBoolFn) -> (ObjCBool) -> ObjCBool>?
var globalObjCBoolFnToBlockBP: @convention(block) (ObjCBoolFn) -> (ObjCBool) -> ObjCBool
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
  func testCBool(_ b: Bool) -> Bool
  func testObjCBool(_ b: Bool) -> Bool
  func testDarwinBoolean(_ b: Bool) -> Bool
  var propCBoolBlock: (Bool) -> Bool
  var propObjCBoolBlock: (Bool) -> Bool
  var propDarwinBooleanBlock: (Bool) -> Bool
  func testCBoolFn(toBlock fp: @convention(c) (Bool) -> Bool) -> (Bool) -> Bool
  func testObjCBoolFn(toBlock fp: @convention(c) (ObjCBool) -> ObjCBool) -> (Bool) -> Bool
  func testDarwinBooleanFn(toBlock fp: @convention(c) (DarwinBoolean) -> DarwinBoolean) -> (Bool) -> Bool
  func produceCBoolBlockTypedef(_ outBlock: AutoreleasingUnsafeMutablePointer<(@convention(block) (Bool) -> Bool)?>)
  func produceObjCBoolBlockTypedef(_ outBlock: AutoreleasingUnsafeMutablePointer<(@convention(block) (ObjCBool) -> ObjCBool)?>)
  func produceDarwinBooleanBlockTypedef(_ outBlock: AutoreleasingUnsafeMutablePointer<(@convention(block) (DarwinBoolean) -> DarwinBoolean)?>)
  init()
}
