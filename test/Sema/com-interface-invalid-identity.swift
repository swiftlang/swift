// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -module-name COM

public struct GUID { }
public typealias IID = GUID

// expected-error@+1{{expected 'COMInterface' to declare 'var IID: IID { get }'}}
public protocol COMInterface {
  // expected-error@+1{{requirement 'unsupported()' of 'COMInterface' is not supported by this compiler}}
  func unsupported()
}
