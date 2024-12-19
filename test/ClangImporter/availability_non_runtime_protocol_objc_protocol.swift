	
// REQUIRES: objc_interop
// REQUIRES: foundation

// RUN: %target-swift-frontend -enable-objc-interop -typecheck -verify  -import-objc-header %S/Inputs/availability_non_runtime_protocol_objc_protocol.h %s

import Foundation

@objc class Wrapper : NSObject {
  @objc public func runMutations(_ mutations: [ObjCNonRuntimeProtocolUsedinSwift]) { // expected-error {{Objective-C protocol 'ObjCNonRuntimeProtocolUsedinSwift' has the non_runtime_protocol attribute and its metadata is not available in Swift}}
    for _ in mutations {
    }
  }

  @objc public func runMutations2(_ mutations: [ObjCRuntimeProtocolUsedinSwift]) {
    for _ in mutations {
    }
  }

  public func iterate(collection mutations: some Collection<any ObjCNonRuntimeProtocolUsedinSwift>) { // expected-error {{Objective-C protocol 'ObjCNonRuntimeProtocolUsedinSwift' has the non_runtime_protocol attribute and its metadata is not available in Swift}}
      for m in mutations {
          print(typeOf(anyNRP: m))
          print(typeOf(someNRP: m))
      }
  }

  public func typeOf(anyNRP nrp: any ObjCNonRuntimeProtocolUsedinSwift) -> Any { // expected-error {{Objective-C protocol 'ObjCNonRuntimeProtocolUsedinSwift' has the non_runtime_protocol attribute and its metadata is not available in Swift}}
      return type(of: nrp)
  }

  public func typeOf(someNRP nrp: some ObjCNonRuntimeProtocolUsedinSwift) -> Any { // expected-error {{Objective-C protocol 'ObjCNonRuntimeProtocolUsedinSwift' has the non_runtime_protocol attribute and its metadata is not available in Swift}}
      return type(of: nrp)
  }

  public func dynamicIsNRP(_ thing: Any) -> Bool {
      return thing is ObjCNonRuntimeProtocolUsedinSwift // expected-error {{Objective-C protocol 'ObjCNonRuntimeProtocolUsedinSwift' has the non_runtime_protocol attribute and its metadata is not available in Swift}}
  }

  @objc protocol ExtendsNRP: ObjCNonRuntimeProtocolUsedinSwift {} // expected-error {{Objective-C protocol 'ObjCNonRuntimeProtocolUsedinSwift' has the non_runtime_protocol attribute and its metadata is not available in Swift}}

  public func dynamicAsNRP(_ thing: ExtendsNRP) -> any ObjCNonRuntimeProtocolUsedinSwift { // expected-error {{Objective-C protocol 'ObjCNonRuntimeProtocolUsedinSwift' has the non_runtime_protocol attribute and its metadata is not available in Swift}}
      return thing as ObjCNonRuntimeProtocolUsedinSwift // expected-error {{Objective-C protocol 'ObjCNonRuntimeProtocolUsedinSwift' has the non_runtime_protocol attribute and its metadata is not available in Swift}}
  }

  public func dynamicAsNRP(_ thing: Any) -> (any ObjCNonRuntimeProtocolUsedinSwift)? { // expected-error {{Objective-C protocol 'ObjCNonRuntimeProtocolUsedinSwift' has the non_runtime_protocol attribute and its metadata is not available in Swift}}
      return thing as? ObjCNonRuntimeProtocolUsedinSwift // expected-error {{Objective-C protocol 'ObjCNonRuntimeProtocolUsedinSwift' has the non_runtime_protocol attribute and its metadata is not available in Swift}}
  }
}