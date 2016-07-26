
struct TclPlatStubs {
  var magic: Int32
  var hooks: OpaquePointer!
  init()
}
var tclPlatStubsPtr: UnsafeMutablePointer<TclPlatStubs>!
