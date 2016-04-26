
@available(tvOS 8.0, *)
enum MTLPurgeableState : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case keepCurrent
  case nonVolatile
  case volatile
  case empty
}
@available(tvOS 8.0, *)
enum MTLCPUCacheMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case defaultCache
  case writeCombined
}
@available(tvOS 9.0, *)
enum MTLStorageMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case shared
  case `private`
}
var MTLResourceCPUCacheModeShift: Int32 { get }
var MTLResourceStorageModeShift: Int32 { get }
@available(tvOS 8.0, *)
struct MTLResourceOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var cpuCacheModeWriteCombined: MTLResourceOptions { get }
  @available(tvOS 9.0, *)
  static var storageModeShared: MTLResourceOptions { get }
  @available(tvOS 9.0, *)
  static var storageModePrivate: MTLResourceOptions { get }
  static var optionCPUCacheModeWriteCombined: MTLResourceOptions { get }
}
@available(tvOS 8.0, *)
protocol MTLResource : NSObjectProtocol {
  var label: String? { get set }
  var device: MTLDevice { get }
  var cpuCacheMode: MTLCPUCacheMode { get }
  @available(tvOS 9.0, *)
  var storageMode: MTLStorageMode { get }
  @discardableResult
  func setPurgeableState(_ state: MTLPurgeableState) -> MTLPurgeableState
}
