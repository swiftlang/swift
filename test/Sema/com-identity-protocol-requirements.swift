// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -module-name COM %t/static.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -module-name COM %t/settable.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -module-name COM %t/wrong-type.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -module-name COM %t/async.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -module-name COM %t/throwing.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -module-name COM %t/missing-iid.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -com-interop-model=microsoft -module-name COM %t/missing-clsid.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -module-name COM %t/associated-type.swift
// RUN: %target-swift-frontend -typecheck -verify -module-name COM %t/disabled.swift

//--- static.swift
public struct IID {}
// expected-error@+1 {{expected 'COMInterface' to declare 'var IID: IID { get }'}}
public protocol COMInterface {
  static var IID: IID { get }
}

//--- settable.swift
public struct IID {}
// expected-error@+1 {{expected 'COMInterface' to declare 'var IID: IID { get }'}}
public protocol COMInterface {
  var IID: IID { get set }
}

//--- wrong-type.swift
public struct IID {}
// expected-error@+1 {{expected 'COMInterface' to declare 'var IID: IID { get }'}}
public protocol COMInterface {
  var IID: Int { get }
}

//--- async.swift
public struct IID {}
// expected-error@+1 {{expected 'COMInterface' to declare 'var IID: IID { get }'}}
public protocol COMInterface {
  var IID: IID { get async }
}

//--- throwing.swift
public struct IID {}
// expected-error@+1 {{expected 'COMInterface' to declare 'var IID: IID { get }'}}
public protocol COMInterface {
  var IID: IID { get throws }
}

//--- missing-iid.swift
// expected-error@+1 {{type 'IID' not found in the 'COM' module}}
public protocol COMInterface {
  var IID: Int { get }
}

//--- missing-clsid.swift
// expected-error@+1 {{type 'CLSID' not found in the 'COM' module}}
public protocol COMActivatable {
  var CLSID: Int { get }
}

//--- associated-type.swift
public struct IID {}
public protocol COMInterface {
  var IID: IID { get }
  // expected-error@+1 {{requirement 'Extra' of 'COMInterface' is not supported by this compiler}}
  associatedtype Extra
}

//--- disabled.swift
// Without COM interop, these names do not impose a compiler-managed contract.
public protocol COMInterface {
  func method()
}
public protocol COMActivatable {
  associatedtype Identity
}
