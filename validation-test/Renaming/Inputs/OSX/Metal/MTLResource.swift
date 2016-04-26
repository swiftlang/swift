
@available(OSX 10.11, *)
enum MTLPurgeableState : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case keepCurrent
  case nonVolatile
  case volatile
  case empty
}
@available(OSX 10.11, *)
enum MTLCPUCacheMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case defaultCache
  case writeCombined
}
@available(OSX 10.11, *)
enum MTLStorageMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case shared
  @available(OSX 10.11, *)
  case managed
  case `private`
}
var MTLResourceCPUCacheModeShift: Int32 { get }
var MTLResourceStorageModeShift: Int32 { get }
@available(OSX 10.11, *)
struct MTLResourceOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var cpuCacheModeWriteCombined: MTLResourceOptions { get }
  @available(OSX 10.11, *)
  static var storageModeShared: MTLResourceOptions { get }
  @available(OSX 10.11, *)
  static var storageModeManaged: MTLResourceOptions { get }
  @available(OSX 10.11, *)
  static var storageModePrivate: MTLResourceOptions { get }
  static var optionCPUCacheModeWriteCombined: MTLResourceOptions { get }
}
@available(OSX 10.11, *)
protocol MTLResource : NSObjectProtocol {
  var label: String? { get set }
  var device: MTLDevice { get }
  var cpuCacheMode: MTLCPUCacheMode { get }
  @available(OSX 10.11, *)
  var storageMode: MTLStorageMode { get }
  @discardableResult
  func setPurgeableState(_ state: MTLPurgeableState) -> MTLPurgeableState
}
