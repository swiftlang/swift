
@available(tvOS 8.0, *)
class CKModifyBadgeOperation : CKOperation {
  convenience init(badgeValue badgeValue: Int)
  var badgeValue: Int
  var modifyBadgeCompletionBlock: ((NSError?) -> Void)?
}
