
@available(iOS 8.4, *)
class MPPlayableContentManagerContext : NSObject {
  var enforcedContentItemsCount: Int { get }
  var enforcedContentTreeDepth: Int { get }
  var contentLimitsEnforced: Bool { get }
  @available(iOS, introduced: 8.4, deprecated: 9.0, message: "Use contentLimitsEnforced")
  var contentLimitsEnabled: Bool { get }
  var endpointAvailable: Bool { get }
}
