
class SKVideoNode : SKNode {
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  init(videoFileNamed videoFile: String)
  @available(OSX 10.10, *)
  init(fileNamed videoFile: String)
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  init(videoURL url: NSURL)
  @available(OSX 10.10, *)
  init(url url: NSURL)
  func play()
  func pause()
  var size: CGSize
  var anchorPoint: CGPoint
}
