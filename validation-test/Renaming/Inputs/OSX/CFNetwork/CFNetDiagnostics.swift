
class CFNetDiagnostic {
}
enum CFNetDiagnosticStatusValues : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case noErr
  case err
  case connectionUp
  case connectionIndeterminate
  case connectionDown
}
typealias CFNetDiagnosticStatus = CFIndex
@available(OSX 10.4, *)
@discardableResult
func CFNetDiagnosticCreateWithStreams(_ alloc: CFAllocator?, _ readStream: CFReadStream?, _ writeStream: CFWriteStream?) -> Unmanaged<CFNetDiagnostic>
@available(OSX 10.4, *)
@discardableResult
func CFNetDiagnosticCreateWithURL(_ alloc: CFAllocator, _ url: CFURL) -> Unmanaged<CFNetDiagnostic>
@available(OSX 10.4, *)
func CFNetDiagnosticSetName(_ details: CFNetDiagnostic, _ name: CFString)
@available(OSX 10.4, *)
@discardableResult
func CFNetDiagnosticDiagnoseProblemInteractively(_ details: CFNetDiagnostic) -> CFNetDiagnosticStatus
@available(OSX 10.4, *)
@discardableResult
func CFNetDiagnosticCopyNetworkStatusPassively(_ details: CFNetDiagnostic, _ description: UnsafeMutablePointer<Unmanaged<CFString>?>?) -> CFNetDiagnosticStatus
