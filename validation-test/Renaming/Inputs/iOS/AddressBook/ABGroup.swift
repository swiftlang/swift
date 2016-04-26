
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNGroup.name")
let kABGroupNameProperty: Int32
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [[CNMutableGroup alloc] init]")
@discardableResult
func ABGroupCreate() -> Unmanaged<ABRecord>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [[CNMutableGroup alloc] init] and [CNSaveRequest addGroup:toContainerWithIdentifier:]")
@discardableResult
func ABGroupCreateInSource(_ source: ABRecord!) -> Unmanaged<ABRecord>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContactStore containersMatchingPredicate:[CNContainer predicateForContainerOfGroupWithIdentifier:] error:]")
@discardableResult
func ABGroupCopySource(_ group: ABRecord!) -> Unmanaged<ABRecord>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContactFetchRequest with predicate = [CNContact predicateForContactsInGroupWithIdentifier:]")
@discardableResult
func ABGroupCopyArrayOfAllMembers(_ group: ABRecord!) -> Unmanaged<CFArray>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContactFetchRequest with predicate = [CNContact predicateForContactsInGroupWithIdentifier:] and sortOrder")
@discardableResult
func ABGroupCopyArrayOfAllMembersWithSortOrdering(_ group: ABRecord!, _ sortOrdering: ABPersonSortOrdering) -> Unmanaged<CFArray>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNSaveRequest addMember:toGroup:]")
@discardableResult
func ABGroupAddMember(_ group: ABRecord!, _ person: ABRecord!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNSaveRequest removeMember:fromGroup:]")
@discardableResult
func ABGroupRemoveMember(_ group: ABRecord!, _ member: ABRecord!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContactStore groupsMatchingPredicate:[CNGroup predicateForGroupsWithIdentifiers:] error:]")
@discardableResult
func ABAddressBookGetGroupWithRecordID(_ addressBook: ABAddressBook!, _ recordID: ABRecordID) -> Unmanaged<ABRecord>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use count of fetch results for [CNContactStore groupsMatchingPredicate:nil error:]")
@discardableResult
func ABAddressBookGetGroupCount(_ addressBook: ABAddressBook!) -> CFIndex
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContactStore groupsMatchingPredicate:nil error:]")
@discardableResult
func ABAddressBookCopyArrayOfAllGroups(_ addressBook: ABAddressBook!) -> Unmanaged<CFArray>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContactStore groupsMatchingPredicate:[CNGroup predicateForGroupsInContainerWithIdentifier:] error:]")
@discardableResult
func ABAddressBookCopyArrayOfAllGroupsInSource(_ addressBook: ABAddressBook!, _ source: ABRecord!) -> Unmanaged<CFArray>!
