
@available(OSX 10.11, *)
class CNSaveRequest : NSObject {
  func add(_ contact: CNMutableContact, toContainerWithIdentifier identifier: String?)
  func update(_ contact: CNMutableContact)
  func delete(_ contact: CNMutableContact)
  func add(_ group: CNMutableGroup, toContainerWithIdentifier identifier: String?)
  func update(_ group: CNMutableGroup)
  func delete(_ group: CNMutableGroup)
  @available(OSX 10.11, *)
  func addSubgroup(_ subgroup: CNGroup, to group: CNGroup)
  @available(OSX 10.11, *)
  func removeSubgroup(_ subgroup: CNGroup, from group: CNGroup)
  func addMember(_ contact: CNContact, to group: CNGroup)
  func removeMember(_ contact: CNContact, from group: CNGroup)
}
