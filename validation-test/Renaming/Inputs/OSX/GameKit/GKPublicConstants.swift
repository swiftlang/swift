
@available(OSX, introduced: 10.8, deprecated: 10.10)
enum GKSendDataMode : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case reliable
  case unreliable
}
@available(OSX, introduced: 10.8, deprecated: 10.10)
enum GKSessionMode : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case server
  case client
  case peer
}
@available(OSX, introduced: 10.8, deprecated: 10.10)
enum GKPeerConnectionState : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case stateAvailable
  case stateUnavailable
  case stateConnected
  case stateDisconnected
  case stateConnecting
}
