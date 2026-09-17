// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -com-interop-model=corefoundation -module-name COM %t/corefoundation-empty.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -com-interop-model=corefoundation -module-name COM %t/corefoundation-unsupported.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -com-interop-model=microsoft -module-name COM %t/microsoft-empty.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -com-interop-model=microsoft -module-name COM %t/microsoft-valid.swift

//--- corefoundation-empty.swift
// CoreFoundation activation has no requirements and does not need a CLSID type.
public protocol COMActivatable {}
func activation<T: COMActivatable>(_: T) {}

//--- corefoundation-unsupported.swift
public struct CLSID {}
public protocol COMActivatable {
  // The Microsoft requirement is not supported in CoreFoundation mode.
  var CLSID: CLSID { get } // expected-error {{requirement 'CLSID' of 'COMActivatable' is not supported by this compiler}}
  func unsupported() // expected-error {{requirement 'unsupported()' of 'COMActivatable' is not supported by this compiler}}
  associatedtype Value // expected-error {{requirement 'Value' of 'COMActivatable' is not supported by this compiler}}
}

//--- microsoft-empty.swift
public struct CLSID {}
public protocol COMActivatable {} // expected-error {{expected 'COMActivatable' to declare 'var CLSID: CLSID { get }'}}

//--- microsoft-valid.swift
public struct CLSID {}
public protocol COMActivatable {
  var CLSID: CLSID { get }
}
