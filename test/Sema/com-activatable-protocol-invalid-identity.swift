// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -com-interop-model=microsoft -module-name COM

public struct GUID { }
public typealias IID = GUID
public typealias CLSID = GUID

public protocol COMInterface {
    var IID: IID { get }
}

// expected-error@+1{{expected 'COMActivatable' to declare 'var CLSID: CLSID { get }'}}
public protocol COMActivatable {
}
