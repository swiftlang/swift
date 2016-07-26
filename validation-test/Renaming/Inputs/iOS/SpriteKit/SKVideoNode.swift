
class SKVideoNode : SKNode {
  @available(iOS, introduced: 7.0, deprecated: 8.0)
  init(videoFileNamed videoFile: String)
  @available(iOS 8.0, *)
  init(fileNamed videoFile: String)
  @available(iOS, introduced: 7.0, deprecated: 8.0)
  init(videoURL url: NSURL)
  @available(iOS 8.0, *)
  init(url url: NSURL)
  func play()
  func pause()
  var size: CGSize
  var anchorPoint: CGPoint
}
