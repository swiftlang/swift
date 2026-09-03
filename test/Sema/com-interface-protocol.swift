// RUN: %target-typecheck-verify-swift -module-name COM -enable-experimental-com-interop

public struct GUID {}
public typealias IID = GUID

public protocol COMInterface {
  var IID: IID { get }

  // expected-error@+1 {{requirement 'unsupportedRequirement()' of 'COMInterface' is not supported by this compiler}}
  func unsupportedRequirement()
}
