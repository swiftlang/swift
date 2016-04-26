
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact, CNGroup, or CNContainer")
typealias ABRecord = CFTypeRef
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CN object's identifier")
typealias ABRecordID = Int32
@available(iOS, introduced: 2.0, deprecated: 9.0)
typealias ABPropertyID = Int32
@available(iOS, introduced: 2.0, deprecated: 9.0)
typealias ABRecordType = UInt32
var kABPersonType: Int { get }
var kABGroupType: Int { get }
var kABSourceType: Int { get }
var kABMultiValueMask: Int32 { get }
var kABRecordInvalidID: Int32 { get }
var kABPropertyInvalidID: Int32 { get }
@available(iOS, introduced: 2.0, deprecated: 9.0)
typealias ABPropertyType = UInt32
var kABInvalidPropertyType: Int { get }
var kABStringPropertyType: Int { get }
var kABIntegerPropertyType: Int { get }
var kABRealPropertyType: Int { get }
var kABDateTimePropertyType: Int { get }
var kABDictionaryPropertyType: Int { get }
var kABMultiStringPropertyType: Int { get }
var kABMultiIntegerPropertyType: Int { get }
var kABMultiRealPropertyType: Int { get }
var kABMultiDateTimePropertyType: Int { get }
var kABMultiDictionaryPropertyType: Int { get }
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CN object's identifier")
@discardableResult
func ABRecordGetRecordID(_ record: ABRecord!) -> ABRecordID
@available(iOS, introduced: 2.0, deprecated: 9.0)
@discardableResult
func ABRecordGetRecordType(_ record: ABRecord!) -> ABRecordType
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CN object's properties")
@discardableResult
func ABRecordCopyValue(_ record: ABRecord!, _ property: ABPropertyID) -> Unmanaged<CFTypeRef>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CN mutable object's properties")
@discardableResult
func ABRecordSetValue(_ record: ABRecord!, _ property: ABPropertyID, _ value: CFTypeRef!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CN mutable object's properties, setting to @, @[], or nil")
@discardableResult
func ABRecordRemoveValue(_ record: ABRecord!, _ property: ABPropertyID, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContactFormatter or CN object's name")
@discardableResult
func ABRecordCopyCompositeName(_ record: ABRecord!) -> Unmanaged<CFString>!
