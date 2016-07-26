
@available(iOS 9.0, *)
class SKReferenceNode : SKNode {
  init(url url: NSURL?)
  init(fileNamed fileName: String?)
  convenience init(fileNamed fileName: String)
  convenience init(url referenceURL: NSURL)
  func didLoad(_ node: SKNode?)
  func resolve()
}
