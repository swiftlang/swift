// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %build-clang-importer-objc-overlays
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk-nosource) -I %t -I %S/Inputs/custom-modules -print-module -source-filename %s -module-to-print=Newtype -skip-unavailable > %t.printed.A.txt
// RUN: %FileCheck %s -check-prefix=PRINT -strict-whitespace < %t.printed.A.txt
// RUN: %target-typecheck-verify-swift -sdk %clang-importer-sdk -I %S/Inputs/custom-modules -I %t -verify-ignore-unknown
// REQUIRES: objc_interop

// PRINT-LABEL: struct ErrorDomain : RawRepresentable, _SwiftNewtypeWrapper, Equatable, Hashable, Comparable, _ObjectiveCBridgeable {
// PRINT-NEXT:    init(_ rawValue: String)
// PRINT-NEXT:    init(rawValue: String)
// PRINT-NEXT:    var _rawValue: NSString
// PRINT-NEXT:    var rawValue: String { get }
// PRINT-NEXT:  }
// PRINT-NEXT:  extension ErrorDomain {
// PRINT-NEXT:    func process()
// PRINT-NEXT:    static let one: ErrorDomain
// PRINT-NEXT:    static let errTwo: ErrorDomain
// PRINT-NEXT:    static let three: ErrorDomain
// PRINT-NEXT:    static let fourErrorDomain: ErrorDomain
// PRINT-NEXT:    static let stillAMember: ErrorDomain
// PRINT-NEXT:  }
// PRINT-NEXT:  struct Food {
// PRINT-NEXT:    init()
// PRINT-NEXT:  }
// PRINT-NEXT:  extension Food {
// PRINT-NEXT:    static let err: ErrorDomain
// PRINT-NEXT:  }
// PRINT-NEXT:  struct ClosedEnum : RawRepresentable, _SwiftNewtypeWrapper, Equatable, Hashable, Comparable, _ObjectiveCBridgeable {
// PRINT-NEXT:    init(rawValue: String)
// PRINT-NEXT:    var _rawValue: NSString
// PRINT-NEXT:    var rawValue: String { get }
// PRINT-NEXT:  }
// PRINT-NEXT:  extension ClosedEnum {
// PRINT-NEXT:    static let firstClosedEntryEnum: ClosedEnum
// PRINT-NEXT:    static let secondEntry: ClosedEnum
// PRINT-NEXT:    static let thirdEntry: ClosedEnum
// PRINT-NEXT:  }
// PRINT-NEXT:  struct IUONewtype : RawRepresentable, _SwiftNewtypeWrapper, Equatable, Hashable, Comparable, _ObjectiveCBridgeable {
// PRINT-NEXT:    init(_ rawValue: String)
// PRINT-NEXT:    init(rawValue: String)
// PRINT-NEXT:    var _rawValue: NSString
// PRINT-NEXT:    var rawValue: String { get }
// PRINT-NEXT:  }
// PRINT-NEXT:  struct MyFloat : RawRepresentable, _SwiftNewtypeWrapper, Equatable, Hashable, Comparable {
// PRINT-NEXT:    init(_ rawValue: Float)
// PRINT-NEXT:    init(rawValue: Float)
// PRINT-NEXT:    let rawValue: Float
// PRINT-NEXT:  }
// PRINT-NEXT:  extension MyFloat {
// PRINT-NEXT:    static let globalFloat: MyFloat{{$}}
// PRINT-NEXT:    static let PI: MyFloat{{$}}
// PRINT-NEXT:    static let version: MyFloat{{$}}
// PRINT-NEXT:  }
//
// PRINT-LABEL: struct MyInt : RawRepresentable, _SwiftNewtypeWrapper, Equatable, Hashable, Comparable {
// PRINT-NEXT:    init(_ rawValue: Int32)
// PRINT-NEXT:    init(rawValue: Int32)
// PRINT-NEXT:    let rawValue: Int32
// PRINT-NEXT:  }
// PRINT-NEXT:  extension MyInt {
// PRINT-NEXT:    static let zero: MyInt{{$}}
// PRINT-NEXT:    static let one: MyInt{{$}}
// PRINT-NEXT:  }
// PRINT-NEXT:  let kRawInt: Int32
// PRINT-NEXT:  func takesMyInt(_: MyInt)
//
// PRINT-LABEL: extension NSURLResourceKey {
// PRINT-NEXT:    static let isRegularFileKey: NSURLResourceKey
// PRINT-NEXT:    static let isDirectoryKey: NSURLResourceKey
// PRINT-NEXT:    static let localizedNameKey: NSURLResourceKey
// PRINT-NEXT:  }
// PRINT-NEXT:  extension NSNotification.Name {
// PRINT-NEXT:    static let Foo: NSNotification.Name
// PRINT-NEXT:    static let bar: NSNotification.Name
// PRINT-NEXT:    static let NSWibble: NSNotification.Name
// PRINT-NEXT:  }
// PRINT-NEXT:  let kNotification: String
// PRINT-NEXT:  let Notification: String
// PRINT-NEXT:  let swiftNamedNotification: String
//
// PRINT-LABEL: struct CFNewType : RawRepresentable, _SwiftNewtypeWrapper {
// PRINT-NEXT:    init(_ rawValue: CFString)
// PRINT-NEXT:    init(rawValue: CFString)
// PRINT-NEXT:    let rawValue: CFString
// PRINT-NEXT:  }
// PRINT-NEXT:  extension CFNewType {
// PRINT-NEXT:    static let MyCFNewTypeValue: CFNewType
// PRINT-NEXT:    static let MyCFNewTypeValueUnauditedButConst: CFNewType
// PRINT-NEXT:    static var MyCFNewTypeValueUnaudited: Unmanaged<CFString>
// PRINT-NEXT:  }
// PRINT-NEXT:  func FooAudited() -> CFNewType
// PRINT-NEXT:  func FooUnaudited() -> Unmanaged<CFString>
//
// PRINT-NEXT:  struct MyABINewType : RawRepresentable, _SwiftNewtypeWrapper {
// PRINT-NEXT:    init(_ rawValue: CFString)
// PRINT-NEXT:    init(rawValue: CFString)
// PRINT-NEXT:    let rawValue: CFString
// PRINT-NEXT:  }
// PRINT-NEXT:  typealias MyABIOldType = CFString
// PRINT-NEXT:  extension MyABINewType {
// PRINT-NEXT:    static let global: MyABINewType!
// PRINT-NEXT:  }
// PRINT-NEXT:  let kMyABIOldTypeGlobal: MyABIOldType!
// PRINT-NEXT:  func getMyABINewType() -> MyABINewType!
// PRINT-NEXT:  func getMyABIOldType() -> MyABIOldType!
// PRINT-NEXT:  func takeMyABINewType(_: MyABINewType!)
// PRINT-NEXT:  func takeMyABIOldType(_: MyABIOldType!)
// PRINT-NEXT:  func takeMyABINewTypeNonNull(_: MyABINewType)
// PRINT-NEXT:  func takeMyABIOldTypeNonNull(_: MyABIOldType)
// PRINT-NEXT:  struct MyABINewTypeNS : RawRepresentable, _SwiftNewtypeWrapper, Equatable, Hashable, Comparable, _ObjectiveCBridgeable {
// PRINT-NEXT:    init(_ rawValue: String)
// PRINT-NEXT:    init(rawValue: String)
// PRINT-NEXT:    var _rawValue: NSString
// PRINT-NEXT:    var rawValue: String { get }
// PRINT-NEXT:  }
// PRINT-NEXT:  typealias MyABIOldTypeNS = NSString
// PRINT-NEXT:  func getMyABINewTypeNS() -> MyABINewTypeNS!
// PRINT-NEXT:  func getMyABIOldTypeNS() -> String!
// PRINT-NEXT:  func takeMyABINewTypeNonNullNS(_: MyABINewTypeNS)
// PRINT-NEXT:  func takeMyABIOldTypeNonNullNS(_: String)
//
// PRINT-NEXT:  struct NSSomeContext {
// PRINT-NEXT:    var i: Int32
// PRINT-NEXT:    init()
// PRINT-NEXT:    init(i: Int32)
// PRINT-NEXT:  }
// PRINT-NEXT:  extension NSSomeContext {
// PRINT-NEXT:    struct Name : RawRepresentable, _SwiftNewtypeWrapper, Equatable, Hashable, Comparable, _ObjectiveCBridgeable {
// PRINT-NEXT:      init(_ rawValue: String)
// PRINT-NEXT:      init(rawValue: String)
// PRINT-NEXT:      var _rawValue: NSString
// PRINT-NEXT:      var rawValue: String { get }
// PRINT-NEXT:    }
// PRINT-NEXT:  }
// PRINT-NEXT:  extension NSSomeContext.Name {
// PRINT-NEXT:    static let myContextName: NSSomeContext.Name
// PRINT-NEXT:  }

import Newtype

func tests() {
	let errOne = ErrorDomain.one
	errOne.process()

	let fooErr = Food.err
	fooErr.process()
	Food().process() // expected-error{{value of type 'Food' has no member 'process'}}

	let thirdEnum = ClosedEnum.thirdEntry
	thirdEnum.process()
	  // expected-error@-1{{value of type 'ClosedEnum' has no member 'process'}}

	let _ = ErrorDomain(rawValue: thirdEnum.rawValue)
	let _ = ClosedEnum(rawValue: errOne.rawValue)

	let _ = NSNotification.Name.Foo
	let _ = NSNotification.Name.bar
	let _ : CFNewType = CFNewType.MyCFNewTypeValue
	let _ : CFNewType = CFNewType.MyCFNewTypeValueUnauditedButConst
	let _ : CFNewType = CFNewType.MyCFNewTypeValueUnaudited
	  // expected-error@-1{{cannot convert value of type 'Unmanaged<CFString>!' to specified type 'CFNewType'}}
}

func acceptSwiftNewtypeWrapper<T : _SwiftNewtypeWrapper>(_ t: T) { }
func acceptEquatable<T : Equatable>(_ t: T) { }
func acceptHashable<T : Hashable>(_ t: T) { }
func acceptComparable<T : Hashable>(_ t: T) { }
func acceptObjectiveCBridgeable<T : _ObjectiveCBridgeable>(_ t: T) { }

func testConformances(ed: ErrorDomain) {
  acceptSwiftNewtypeWrapper(ed)
  acceptEquatable(ed)
  acceptHashable(ed)
  acceptComparable(ed)
  acceptObjectiveCBridgeable(ed)
}

func testFixit() {
	let _ = NSMyContextName
	  // expected-error@-1{{'NSMyContextName' has been renamed to 'NSSomeContext.Name.myContextName'}} {{10-25=NSSomeContext.Name.myContextName}}
}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected note produced: 'NSMyContextName' was obsoleted in Swift 3
