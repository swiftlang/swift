
@available(iOS 8.0, *)
class PHFetchOptions : NSObject, NSCopying {
  var predicate: NSPredicate?
  var sortDescriptors: [NSSortDescriptor]?
  var includeHiddenAssets: Bool
  var includeAllBurstAssets: Bool
  @available(iOS 9.0, *)
  var includeAssetSourceTypes: PHAssetSourceType
  @available(iOS 9.0, *)
  var fetchLimit: Int
  var wantsIncrementalChangeDetails: Bool
}
