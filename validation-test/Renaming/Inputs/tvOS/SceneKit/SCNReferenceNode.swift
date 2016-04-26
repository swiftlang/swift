
@available(tvOS 9.0, *)
enum SCNReferenceLoadingPolicy : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case immediate
  case onDemand
}
@available(tvOS 9.0, *)
class SCNReferenceNode : SCNNode {
  init?(url referenceURL: NSURL)
  @NSCopying var referenceURL: NSURL
  var loadingPolicy: SCNReferenceLoadingPolicy
  func load()
  func unload()
  var isLoaded: Bool { get }
}
