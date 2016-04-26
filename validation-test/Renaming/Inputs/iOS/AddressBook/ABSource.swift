
var kABSourceTypeSearchableMask: Int32 { get }
var kABSourceTypeLocal: Int { get }
var kABSourceTypeExchange: Int { get }
var kABSourceTypeExchangeGAL: Int { get }
var kABSourceTypeMobileMe: Int { get }
var kABSourceTypeLDAP: Int { get }
var kABSourceTypeCardDAV: Int { get }
var kABSourceTypeCardDAVSearch: Int { get }
@available(iOS, introduced: 2.0, deprecated: 9.0)
typealias ABSourceType = Int32
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContainer.name")
let kABSourceNameProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContainer.type")
let kABSourceTypeProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContactStore containersMatchingPredicate:[CNContainer predicateForContainersWithIdentifiers: @[CNContactStore.defaultContainerIdentifier]] error:]")
@discardableResult
func ABAddressBookCopyDefaultSource(_ addressBook: ABAddressBook!) -> Unmanaged<ABRecord>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContactStore containersMatchingPredicate:[CNContainer predicateForContainersWithIdentifiers:] error:]")
@discardableResult
func ABAddressBookGetSourceWithRecordID(_ addressBook: ABAddressBook!, _ sourceID: ABRecordID) -> Unmanaged<ABRecord>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContactStore containersMatchingPredicate:nil error:]")
@discardableResult
func ABAddressBookCopyArrayOfAllSources(_ addressBook: ABAddressBook!) -> Unmanaged<CFArray>!
