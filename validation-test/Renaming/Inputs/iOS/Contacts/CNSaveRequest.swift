
@available(iOS 9.0, *)
class CNSaveRequest : NSObject {
  func add(_ contact: CNMutableContact, toContainerWithIdentifier identifier: String?)
  func update(_ contact: CNMutableContact)
  func delete(_ contact: CNMutableContact)
  func add(_ group: CNMutableGroup, toContainerWithIdentifier identifier: String?)
  func update(_ group: CNMutableGroup)
  func delete(_ group: CNMutableGroup)
  func addMember(_ contact: CNContact, to group: CNGroup)
  func removeMember(_ contact: CNContact, from group: CNGroup)
}
