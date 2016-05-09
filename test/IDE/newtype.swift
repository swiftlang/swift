// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %build-clang-importer-objc-overlays
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk-nosource) -I %t -I %S/Inputs/custom-modules -enable-swift-newtype -print-module -source-filename %s -module-to-print=Newtype > %t.printed.A.txt
// RUN: FileCheck %s -check-prefix=PRINT -strict-whitespace < %t.printed.A.txt
// RUN: %target-parse-verify-swift -sdk %clang-importer-sdk -I %S/Inputs/custom-modules -enable-swift-newtype -I %t
// REQUIRES: objc_interop

// PRINT-LABEL: struct ErrorDomain : RawRepresentable, _SwiftNewtypeWrapper, Equatable, Hashable, Comparable, _ObjectiveCBridgeable {
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
// PRINT-NEXT:    init(rawValue: String)
// PRINT-NEXT:    var _rawValue: NSString
// PRINT-NEXT:    var rawValue: String { get }
// PRINT-NEXT:  }
// PRINT-NEXT:  struct MyFloat : RawRepresentable, _SwiftNewtypeWrapper, Equatable, Hashable, Comparable {
// PRINT-NEXT:    init(rawValue: Float)
// PRINT-NEXT:    let rawValue: Float
// PRINT-NEXT:  }
// PRINT-NEXT:  extension MyFloat {
// PRINT-NEXT:    static let globalFloat: MyFloat
// PRINT-NEXT:    static let PI: MyFloat
// PRINT-NEXT:    static let version: MyFloat
// PRINT-NEXT:  }
//
// PRINT-LABEL: extension NSURLResourceKey {
// PRINT-NEXT:    static let isRegularFileKey: NSURLResourceKey
// PRINT-NEXT:    static let isDirectoryKey: NSURLResourceKey
// PRINT-NEXT:    static let localizedNameKey: NSURLResourceKey
// PRINT-NEXT:  }
// PRINT-NEXT:  extension NSNotificationName {
// PRINT-NEXT:    static let Foo: NSNotificationName
// PRINT-NEXT:    static let bar: NSNotificationName
// PRINT-NEXT:    static let NSWibble: NSNotificationName
// PRINT-NEXT:  }
// PRINT-NEXT:  let kNotification: String
// PRINT-NEXT:  let Notification: String
// PRINT-NEXT:  let swiftNamedNotification: String
//
// PRINT-LABEL: struct CFNewType : RawRepresentable, _SwiftNewtypeWrapper {
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
import Newtype

func tests() {
	let errOne = ErrorDomain.one
	errOne.process()

	let fooErr = Food.err
	fooErr.process()
	Food().process() // expected-error{{value of type 'Food' has no member 'process'}}

	let thirdEnum = ClosedEnum.thirdEntry!
	thirdEnum.process()
	  // expected-error@-1{{value of type 'ClosedEnum' has no member 'process'}}

	let _ = ErrorDomain(rawValue: thirdEnum.rawValue)
	let _ = ClosedEnum(rawValue: errOne.rawValue)

	let _ = NSNotificationName.Foo
	let _ = NSNotificationName.bar
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
