
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNErrorDomain")
let ABAddressBookErrorDomain: CFString!
var kABOperationNotPermittedByStoreError: Int { get }
var kABOperationNotPermittedByUserError: Int { get }
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContactStore")
typealias ABAddressBook = CFTypeRef
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNAuthorizationStatus")
enum ABAuthorizationStatus : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case notDetermined
  case restricted
  case denied
  case authorized
}
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContactStore authorizationStatusForEntityType:]")
@discardableResult
func ABAddressBookGetAuthorizationStatus() -> ABAuthorizationStatus
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [[CNContactStore alloc] init]")
@discardableResult
func ABAddressBookCreateWithOptions(_ options: CFDictionary!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Unmanaged<ABAddressBook>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [[CNContactStore alloc] init]")
@discardableResult
func ABAddressBookCreate() -> Unmanaged<ABAddressBook>!
@available(iOS, introduced: 2.0, deprecated: 9.0)
typealias ABAddressBookRequestAccessCompletionHandler = (Bool, CFError!) -> Void
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContactStore requestAccessForEntityType:completionHandler:]")
func ABAddressBookRequestAccessWithCompletion(_ addressBook: ABAddressBook!, _ completion: ABAddressBookRequestAccessCompletionHandler!)
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContactStore executeSaveRequest:error:]")
@discardableResult
func ABAddressBookSave(_ addressBook: ABAddressBook!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(iOS, introduced: 2.0, deprecated: 9.0)
@discardableResult
func ABAddressBookHasUnsavedChanges(_ addressBook: ABAddressBook!) -> Bool
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNSaveRequest")
@discardableResult
func ABAddressBookAddRecord(_ addressBook: ABAddressBook!, _ record: ABRecord!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNSaveRequest")
@discardableResult
func ABAddressBookRemoveRecord(_ addressBook: ABAddressBook!, _ record: ABRecord!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNLabeledValue localizedStringForLabel:]")
@discardableResult
func ABAddressBookCopyLocalizedLabel(_ label: CFString!) -> Unmanaged<CFString>!
@available(iOS, introduced: 2.0, deprecated: 9.0)
typealias ABExternalChangeCallback = @convention(c) (ABAddressBook!, CFDictionary!, UnsafeMutablePointer<Void>!) -> Void
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContactStoreDidChangeNotification")
func ABAddressBookRegisterExternalChangeCallback(_ addressBook: ABAddressBook!, _ callback: ABExternalChangeCallback!, _ context: UnsafeMutablePointer<Void>!)
@available(iOS, introduced: 2.0, deprecated: 9.0)
func ABAddressBookUnregisterExternalChangeCallback(_ addressBook: ABAddressBook!, _ callback: ABExternalChangeCallback!, _ context: UnsafeMutablePointer<Void>!)
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "refetch CN objects")
func ABAddressBookRevert(_ addressBook: ABAddressBook!)
