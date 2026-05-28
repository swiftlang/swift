// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print=BoolBridgingTests -function-definitions=false -skip-unavailable -F %S/Inputs/mock-sdk > %t.txt
// RUN: tail -n +10 %s > %t/a
// RUN: diff -u %t/a %t.txt

// REQUIRES: objc_interop

// EXPECTED OUTPUT STARTS BELOW THIS LINE.
@_exported import Foundation

func testCBool(_: CBool) -> CBool
func testObjCBool(_: Bool) -> Bool
func testDarwinBoolean(_: Bool) -> Bool
typealias CBoolTypedef = CBool
typealias ObjCBoolTypedef = ObjCBool
typealias DarwinBooleanTypedef = DarwinBoolean
func testCBoolTypedef(_: CBoolTypedef) -> CBoolTypedef
func testObjCBoolTypedef(_: Bool) -> Bool
func testDarwinBooleanTypedef(_: Bool) -> Bool
func testCBoolPointer(_: UnsafeMutablePointer<CBool>) -> UnsafePointer<CBool>
func testObjCBoolPointer(_: UnsafeMutablePointer<ObjCBool>) -> UnsafePointer<ObjCBool>
func testDarwinBooleanPointer(_: UnsafeMutablePointer<DarwinBoolean>) -> UnsafePointer<DarwinBoolean>
typealias CBoolFn = @convention(c) (CBool) -> CBool
typealias ObjCBoolFn = @convention(c) (ObjCBool) -> ObjCBool
typealias DarwinBooleanFn = @convention(c) (DarwinBoolean) -> DarwinBoolean
typealias CBoolBlock = (CBool) -> CBool
typealias ObjCBoolBlock = (Bool) -> Bool
typealias DarwinBooleanBlock = (Bool) -> Bool
func testCBoolFnToBlock(_: @convention(c) (CBool) -> CBool) -> (CBool) -> CBool
func testObjCBoolFnToBlock(_: @convention(c) (ObjCBool) -> ObjCBool) -> (Bool) -> Bool
func testDarwinBooleanFnToBlock(_: @convention(c) (DarwinBoolean) -> DarwinBoolean) -> (Bool) -> Bool
func testCBoolFnToBlockTypedef(_: CBoolFn) -> CBoolBlock
func testObjCBoolFnToBlockTypedef(_: ObjCBoolFn) -> ObjCBoolBlock
func testDarwinBooleanFnToBlockTypedef(_: DarwinBooleanFn) -> DarwinBooleanBlock
typealias CBoolFnToBlockType = (CBoolFn) -> (CBool) -> CBool
typealias ObjCBoolFnToBlockType = (ObjCBoolFn) -> (ObjCBool) -> ObjCBool
typealias DarwinBooleanFnToBlockType = (DarwinBooleanFn) -> (DarwinBoolean) -> DarwinBoolean
var globalObjCBoolFnToBlockFP: @convention(c) (ObjCBoolFn) -> (ObjCBool) -> ObjCBool
var globalObjCBoolFnToBlockFPP: UnsafeMutablePointer<@convention(c) (ObjCBoolFn) -> (ObjCBool) -> ObjCBool>?
var globalObjCBoolFnToBlockBP: @convention(block) (ObjCBoolFn) -> (ObjCBool) -> ObjCBool
var globalCBoolFn: CBoolFn
var globalObjCBoolFn: ObjCBoolFn
var globalDarwinBooleanFn: DarwinBooleanFn
var globalCBoolBlock: @convention(block) (CBool) -> CBool
var globalObjCBoolBlock: @convention(block) (ObjCBool) -> ObjCBool
var globalDarwinBooleanBlock: @convention(block) (DarwinBoolean) -> DarwinBoolean
class Test : NSObject {
  var propCBool: CBool
  var propObjCBool: Bool
  var propDarwinBoolean: Bool
  func testCBool(_ b: CBool) -> CBool
  func testObjCBool(_ b: Bool) -> Bool
  func testDarwinBoolean(_ b: Bool) -> Bool
  var propCBoolBlock: (CBool) -> CBool
  var propObjCBoolBlock: (Bool) -> Bool
  var propDarwinBooleanBlock: (Bool) -> Bool
  func testCBoolFn(toBlock fp: @convention(c) (CBool) -> CBool) -> (CBool) -> CBool
  func testObjCBoolFn(toBlock fp: @convention(c) (ObjCBool) -> ObjCBool) -> (Bool) -> Bool
  func testDarwinBooleanFn(toBlock fp: @convention(c) (DarwinBoolean) -> DarwinBoolean) -> (Bool) -> Bool
  func produceCBoolBlockTypedef(_ outBlock: AutoreleasingUnsafeMutablePointer<(@convention(block) (CBool) -> CBool)?>)
  func produceObjCBoolBlockTypedef(_ outBlock: AutoreleasingUnsafeMutablePointer<(@convention(block) (ObjCBool) -> ObjCBool)?>)
  func produceDarwinBooleanBlockTypedef(_ outBlock: AutoreleasingUnsafeMutablePointer<(@convention(block) (DarwinBoolean) -> DarwinBoolean)?>)
  init()
}
