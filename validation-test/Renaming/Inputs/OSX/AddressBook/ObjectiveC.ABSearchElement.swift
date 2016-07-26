
class ABSearchElement : NSObject {
  /*not inherited*/ init!(forConjunction conjuction: ABSearchConjunction, children children: [AnyObject]!)
  @discardableResult
  func matchesRecord(_ record: ABRecord!) -> Bool
}
