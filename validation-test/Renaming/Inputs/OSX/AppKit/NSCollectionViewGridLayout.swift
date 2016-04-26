
@available(OSX 10.11, *)
class NSCollectionViewGridLayout : NSCollectionViewLayout {
  var margins: NSEdgeInsets
  var minimumInteritemSpacing: CGFloat
  var minimumLineSpacing: CGFloat
  var maximumNumberOfRows: Int
  var maximumNumberOfColumns: Int
  var minimumItemSize: NSSize
  var maximumItemSize: NSSize
  var backgroundColors: [NSColor]!
}
