// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk-nosource) -I %t -I %S/Inputs/custom-modules -print-module -source-filename %s -module-to-print=Newtype -skip-unavailable -access-filter-public > %t.printed.A.txt
// RUN: %FileCheck %s -check-prefix=PRINT -strict-whitespace < %t.printed.A.txt
// RUN: %target-typecheck-verify-swift -sdk %clang-importer-sdk -I %S/Inputs/custom-modules -I %t
// REQUIRES: objc_interop

// PRINT-LABEL: struct ErrorDomain : _ObjectiveCBridgeable, Hashable, Equatable, _SwiftNewtypeWrapper, RawRepresentable {
// PRINT-NEXT:    init(_ rawValue: String)
// PRINT-NEXT:    init(rawValue: String)
// PRINT-NEXT:    var rawValue: String { get }
// PRINT-NEXT:    typealias RawValue = String
// PRINT-NEXT:    typealias _ObjectiveCType = NSString
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
// PRINT-NEXT:  struct ClosedEnum : _ObjectiveCBridgeable, Hashable, Equatable, _SwiftNewtypeWrapper, RawRepresentable {
// PRINT-NEXT:    init(rawValue: String)
// PRINT-NEXT:    var rawValue: String { get }
// PRINT-NEXT:    typealias RawValue = String
// PRINT-NEXT:    typealias _ObjectiveCType = NSString
// PRINT-NEXT:  }
// PRINT-NEXT:  extension ClosedEnum {
// PRINT-NEXT:    static let firstClosedEntryEnum: ClosedEnum
// PRINT-NEXT:    static let secondEntry: ClosedEnum
// PRINT-NEXT:    static let thirdEntry: ClosedEnum
// PRINT-NEXT:  }
// PRINT-NEXT:  struct IUONewtype : _ObjectiveCBridgeable, Hashable, Equatable, _SwiftNewtypeWrapper, RawRepresentable {
// PRINT-NEXT:    init(_ rawValue: String)
// PRINT-NEXT:    init(rawValue: String)
// PRINT-NEXT:    var rawValue: String { get }
// PRINT-NEXT:    typealias RawValue = String
// PRINT-NEXT:    typealias _ObjectiveCType = NSString
// PRINT-NEXT:  }
// PRINT-NEXT:  struct MyFloat : Hashable, Equatable, _SwiftNewtypeWrapper, RawRepresentable {
// PRINT-NEXT:    init(_ rawValue: Float)
// PRINT-NEXT:    init(rawValue: Float)
// PRINT-NEXT:    let rawValue: Float
// PRINT-NEXT:    typealias RawValue = Float
// PRINT-NEXT:  }
// PRINT-NEXT:  extension MyFloat {
// PRINT-NEXT:    static let globalFloat: MyFloat{{$}}
// PRINT-NEXT:    static let PI: MyFloat{{$}}
// PRINT-NEXT:    static let version: MyFloat{{$}}
// PRINT-NEXT:  }
//
// PRINT-LABEL: struct MyInt : Hashable, Equatable, _SwiftNewtypeWrapper, RawRepresentable {
// PRINT-NEXT:    init(_ rawValue: Int32)
// PRINT-NEXT:    init(rawValue: Int32)
// PRINT-NEXT:    let rawValue: Int32
// PRINT-NEXT:    typealias RawValue = Int32
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
// PRINT-LABEL: struct CFNewType : Hashable, Equatable, _SwiftNewtypeWrapper, RawRepresentable {
// PRINT-NEXT:    init(_ rawValue: CFString)
// PRINT-NEXT:    init(rawValue: CFString)
// PRINT-NEXT:    let rawValue: CFString
// PRINT-NEXT:    typealias RawValue = CFString
// PRINT-NEXT:  }
// PRINT-NEXT:  extension CFNewType {
// PRINT-NEXT:    static let MyCFNewTypeValue: CFNewType
// PRINT-NEXT:    static let MyCFNewTypeValueUnauditedButConst: CFNewType
// PRINT-NEXT:    static var MyCFNewTypeValueUnaudited: Unmanaged<CFString>
// PRINT-NEXT:  }
// PRINT-NEXT:  func FooAudited() -> CFNewType
// PRINT-NEXT:  func FooUnaudited() -> Unmanaged<CFString>
//
// PRINT-LABEL: struct MyABINewType : Hashable, Equatable, _SwiftNewtypeWrapper, RawRepresentable {
// PRINT-NEXT:    init(_ rawValue: CFString)
// PRINT-NEXT:    init(rawValue: CFString)
// PRINT-NEXT:    let rawValue: CFString
// PRINT-NEXT:    typealias RawValue = CFString
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
// PRINT-LABEL: struct MyABINewTypeNS : _ObjectiveCBridgeable, Hashable, Equatable, _SwiftNewtypeWrapper, RawRepresentable {
// PRINT-NEXT:    init(_ rawValue: String)
// PRINT-NEXT:    init(rawValue: String)
// PRINT-NEXT:    var rawValue: String { get }
// PRINT-NEXT:    typealias RawValue = String
// PRINT-NEXT:    typealias _ObjectiveCType = NSString
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
// PRINT-NEXT:    struct Name : _ObjectiveCBridgeable, Hashable, Equatable, _SwiftNewtypeWrapper, RawRepresentable {
// PRINT-NEXT:      init(_ rawValue: String)
// PRINT-NEXT:      init(rawValue: String)
// PRINT-NEXT:      var rawValue: String { get }
// PRINT-NEXT:      typealias RawValue = String
// PRINT-NEXT:      typealias _ObjectiveCType = NSString
// PRINT-NEXT:    }
// PRINT-NEXT:  }
// PRINT-NEXT:  extension NSSomeContext.Name {
// PRINT-NEXT:    static let myContextName: NSSomeContext.Name
// PRINT-NEXT:  }
//
// PRINT-NEXT: struct TRef : Hashable, Equatable, _SwiftNewtypeWrapper, RawRepresentable {
// PRINT-NEXT:   init(_ rawValue: OpaquePointer)
// PRINT-NEXT:   init(rawValue: OpaquePointer)
// PRINT-NEXT:   let rawValue: OpaquePointer
// PRINT-NEXT:   typealias RawValue = OpaquePointer
// PRINT-NEXT: }
// PRINT-NEXT: struct ConstTRef : Hashable, Equatable, _SwiftNewtypeWrapper, RawRepresentable {
// PRINT-NEXT:   init(_ rawValue: OpaquePointer)
// PRINT-NEXT:   init(rawValue: OpaquePointer)
// PRINT-NEXT:   let rawValue: OpaquePointer
// PRINT-NEXT:   typealias RawValue = OpaquePointer
// PRINT-NEXT: }
// PRINT-NEXT: func create_T() -> TRef
// PRINT-NEXT: func create_ConstT() -> ConstTRef
// PRINT-NEXT: func destroy_T(_: TRef!)
// PRINT-NEXT: func destroy_ConstT(_: ConstTRef!)
// PRINT-NEXT: extension TRef {
// PRINT-NEXT:   func mutatePointee()
// PRINT-NEXT:   mutating func mutate()
// PRINT-NEXT: }
// PRINT-NEXT: extension ConstTRef {
// PRINT-NEXT:   func use()
// PRINT-NEXT: }
//
// PRINT-NEXT: struct TRefRef : Hashable, Equatable, _SwiftNewtypeWrapper, RawRepresentable {
// PRINT-NEXT:   init(_ rawValue: UnsafeMutablePointer<OpaquePointer>)
// PRINT-NEXT:   init(rawValue: UnsafeMutablePointer<OpaquePointer>)
// PRINT-NEXT:   let rawValue: UnsafeMutablePointer<OpaquePointer>
// PRINT-NEXT:   typealias RawValue = UnsafeMutablePointer<OpaquePointer>
// PRINT-NEXT: }
// PRINT-NEXT: struct ConstTRefRef : Hashable, Equatable, _SwiftNewtypeWrapper, RawRepresentable {
// PRINT-NEXT:   init(_ rawValue: UnsafePointer<OpaquePointer>)
// PRINT-NEXT:   init(rawValue: UnsafePointer<OpaquePointer>)
// PRINT-NEXT:   let rawValue: UnsafePointer<OpaquePointer>
// PRINT-NEXT:   typealias RawValue = UnsafePointer<OpaquePointer>
// PRINT-NEXT: }
// PRINT-NEXT: func create_TRef() -> TRefRef
// PRINT-NEXT: func create_ConstTRef() -> ConstTRefRef
// PRINT-NEXT: func destroy_TRef(_: TRefRef!)
// PRINT-NEXT: func destroy_ConstTRef(_: ConstTRefRef!)
// PRINT-NEXT: extension TRefRef {
// PRINT-NEXT:   func mutatePointee()
// PRINT-NEXT:   mutating func mutate()
// PRINT-NEXT: }
// PRINT-NEXT: extension ConstTRefRef {
// PRINT-NEXT:   func use()
// PRINT-NEXT: }

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
	  // expected-error@-1{{cannot convert value of type 'Unmanaged<CFString>?' to specified type 'CFNewType'}}
}

func acceptSwiftNewtypeWrapper<T : _SwiftNewtypeWrapper>(_ t: T) { }
func acceptEquatable<T : Equatable>(_ t: T) { }
func acceptHashable<T : Hashable>(_ t: T) { }
func acceptObjectiveCBridgeable<T : _ObjectiveCBridgeable>(_ t: T) { }

func testConformances(ed: ErrorDomain) {
  acceptSwiftNewtypeWrapper(ed)
  acceptEquatable(ed)
  acceptHashable(ed)
  acceptObjectiveCBridgeable(ed)
}

func testFixit() {
	let _ = NSMyContextName
	  // expected-error@-1{{'NSMyContextName' has been renamed to 'NSSomeContext.Name.myContextName'}} {{10-25=NSSomeContext.Name.myContextName}}
}

func testNonEphemeralInitParams(x: OpaquePointer) {
  var x = x

  _ = TRefRef(&x) // expected-warning {{inout expression creates a temporary pointer, but argument #1 should be a pointer that outlives the call to 'init(_:)'}}
  // expected-note@-1 {{implicit argument conversion from 'OpaquePointer' to 'UnsafeMutablePointer<OpaquePointer>' produces a pointer valid only for the duration of the call}}
  // expected-note@-2 {{use 'withUnsafeMutablePointer' in order to explicitly convert argument to pointer valid for a defined scope}}

  _ = TRefRef(rawValue: &x) // expected-warning {{inout expression creates a temporary pointer, but argument 'rawValue' should be a pointer that outlives the call to 'init(rawValue:)'}}
  // expected-note@-1 {{implicit argument conversion from 'OpaquePointer' to 'UnsafeMutablePointer<OpaquePointer>' produces a pointer valid only for the duration of the call}}
  // expected-note@-2 {{use 'withUnsafeMutablePointer' in order to explicitly convert argument to pointer valid for a defined scope}}

  _ = ConstTRefRef(&x) // expected-warning {{inout expression creates a temporary pointer, but argument #1 should be a pointer that outlives the call to 'init(_:)'}}
  // expected-note@-1 {{implicit argument conversion from 'OpaquePointer' to 'UnsafePointer<OpaquePointer>' produces a pointer valid only for the duration of the call}}
  // expected-note@-2 {{use 'withUnsafePointer' in order to explicitly convert argument to pointer valid for a defined scope}}

  _ = ConstTRefRef(rawValue: &x) // expected-warning {{inout expression creates a temporary pointer, but argument 'rawValue' should be a pointer that outlives the call to 'init(rawValue:)'}}
  // expected-note@-1 {{implicit argument conversion from 'OpaquePointer' to 'UnsafePointer<OpaquePointer>' produces a pointer valid only for the duration of the call}}
  // expected-note@-2 {{use 'withUnsafePointer' in order to explicitly convert argument to pointer valid for a defined scope}}
}
