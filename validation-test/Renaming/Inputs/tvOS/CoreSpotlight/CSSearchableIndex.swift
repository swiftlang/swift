
extension CSSearchableIndex {
  func beginBatch()
  func endBatch(withClientState clientState: NSData, completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  func fetchLastClientState(completionHandler completionHandler: (NSData?, NSError?) -> Void)
}
