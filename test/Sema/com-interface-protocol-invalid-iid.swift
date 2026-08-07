// RUN: %target-typecheck-verify-swift -module-name COM -enable-experimental-com-interop

public struct GUID {}
public typealias IID = GUID

// expected-error@+1 {{expected 'COMInterface' to declare 'var IID: IID { get }'}}
public protocol COMInterface {
}
