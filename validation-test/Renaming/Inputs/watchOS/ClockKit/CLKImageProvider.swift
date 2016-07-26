
class CLKImageProvider : NSObject, NSCopying {
  convenience init(onePieceImage onePieceImage: UIImage)
  convenience init(onePieceImage onePieceImage: UIImage, twoPieceImageBackground twoPieceImageBackground: UIImage?, twoPieceImageForeground twoPieceImageForeground: UIImage?)
  var onePieceImage: UIImage
  var tintColor: UIColor?
  var twoPieceImageBackground: UIImage?
  var twoPieceImageForeground: UIImage?
  var accessibilityLabel: String?
}
