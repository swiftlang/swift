
@available(OSX 10.8, *)
struct AVB17221EntityPropertyChanged : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var timeToLive: AVB17221EntityPropertyChanged { get }
  @available(OSX 10.9, *)
  static var entityID: AVB17221EntityPropertyChanged { get }
  static var vendorID: AVB17221EntityPropertyChanged { get }
  static var modelID: AVB17221EntityPropertyChanged { get }
  static var entityCapabilities: AVB17221EntityPropertyChanged { get }
  static var talkerStreamSources: AVB17221EntityPropertyChanged { get }
  static var talkerCapabilities: AVB17221EntityPropertyChanged { get }
  static var listenerStreamSinks: AVB17221EntityPropertyChanged { get }
  static var listenerCapabilities: AVB17221EntityPropertyChanged { get }
  static var controllerCapabilities: AVB17221EntityPropertyChanged { get }
  static var availableIndex: AVB17221EntityPropertyChanged { get }
  @available(OSX 10.9, *)
  static var gptpGrandmasterID: AVB17221EntityPropertyChanged { get }
  static var macAddress: AVB17221EntityPropertyChanged { get }
  static var associationID: AVB17221EntityPropertyChanged { get }
  static var entityType: AVB17221EntityPropertyChanged { get }
  @available(OSX 10.9, *)
  static var identifyControlIndex: AVB17221EntityPropertyChanged { get }
  @available(OSX 10.9, *)
  static var interfaceIndex: AVB17221EntityPropertyChanged { get }
  @available(OSX 10.9, *)
  static var gptpDomainNumber: AVB17221EntityPropertyChanged { get }
}
protocol AVB17221EntityDiscoveryDelegate {
  @available(OSX 10.8, *)
  func didAddRemoteEntity(_ newEntity: AVB17221Entity, on17221EntityDiscovery entityDiscovery: AVB17221EntityDiscovery)
  @available(OSX 10.8, *)
  func didRemoveRemoteEntity(_ oldEntity: AVB17221Entity, on17221EntityDiscovery entityDiscovery: AVB17221EntityDiscovery)
  @available(OSX 10.8, *)
  func didRediscoverRemoteEntity(_ entity: AVB17221Entity, on17221EntityDiscovery entityDiscovery: AVB17221EntityDiscovery)
  @available(OSX 10.8, *)
  func didUpdateRemoteEntity(_ entity: AVB17221Entity, changedProperties changedProperties: AVB17221EntityPropertyChanged, on17221EntityDiscovery entityDiscovery: AVB17221EntityDiscovery)
  @available(OSX 10.8, *)
  func didAddLocalEntity(_ newEntity: AVB17221Entity, on17221EntityDiscovery entityDiscovery: AVB17221EntityDiscovery)
  @available(OSX 10.8, *)
  func didRemoveLocalEntity(_ oldEntity: AVB17221Entity, on17221EntityDiscovery entityDiscovery: AVB17221EntityDiscovery)
  @available(OSX 10.8, *)
  func didRediscoverLocalEntity(_ entity: AVB17221Entity, on17221EntityDiscovery entityDiscovery: AVB17221EntityDiscovery)
  @available(OSX 10.8, *)
  func didUpdateLocalEntity(_ entity: AVB17221Entity, changedProperties changedProperties: AVB17221EntityPropertyChanged, on17221EntityDiscovery entityDiscovery: AVB17221EntityDiscovery)
}
