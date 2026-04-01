// Crash reproducer from https://github.com/swiftlang/swift/issues/77047#issuecomment-2440103712
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir %t/artifacts

// RUN: %target-swift-frontend -c %t/ChibiStdlib.swift -parse-stdlib -emit-module -emit-module-path %t/sdk/usr/lib/swift/Swift.swiftmodule/%module-target-triple.swiftmodule -module-name Swift -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -o %t/artifacts
// RUN: %target-swift-frontend -c %t/XMLParser.swift -parse-as-library -emit-module -emit-module-path %t/sdk/usr/lib/swift/MyFoundationXML.swiftmodule/%module-target-triple.swiftmodule -module-name MyFoundationXML -module-link-name MyFoundationXML -resource-dir %t/sdk -O -I %t/include -o %t/artifacts  -sdk %t/sdk
// RUN: %target-swift-frontend -c %t/Check.swift -o %t/artifacts -index-store-path %t/index-store -index-system-modules -resource-dir %t/sdk -parse-stdlib -sdk %t/sdk

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir %t/artifacts

// RUN: %target-swift-frontend -c %t/ChibiStdlib.swift -parse-stdlib -emit-module -emit-module-path %t/sdk/usr/lib/swift/Swift.swiftmodule/%module-target-triple.swiftmodule -module-name Swift -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -o %t/artifacts
// RUN: %target-swift-frontend -c %t/XMLParser.swift -enable-library-evolution -parse-as-library -emit-module -emit-module-path %t/sdk/usr/lib/swift/MyFoundationXML.swiftmodule/%module-target-triple.swiftmodule -module-name MyFoundationXML -module-link-name MyFoundationXML -resource-dir %t/sdk -O -I %t/include -o %t/artifacts  -sdk %t/sdk
// RUN: %target-swift-frontend -c %t/Check.swift -o %t/artifacts -index-store-path %t/index-store -index-system-modules -resource-dir %t/sdk -parse-stdlib -sdk %t/sdk


//--- Check.swift
import MyFoundationXML

//--- XMLParser.swift
@_implementationOnly import _MyCFXMLInterface

func _NSXMLParserExternalEntityWithURL(originalLoaderFunction: _CFXMLInterfaceExternalEntityLoader) {}

//--- include/module.modulemap
module _MyCFXMLInterface {
  header "MyCFXMLInterface.h"
}

//--- include/MyCFXMLInterface.h
#if !defined(__COREFOUNDATION_CFXMLINTERFACE__)
#define __COREFOUNDATION_CFXMLINTERFACE__ 1

typedef struct _xmlParserCtxt *_CFXMLInterfaceParserContext;
typedef void (*_CFXMLInterfaceExternalEntityLoader)(_CFXMLInterfaceParserContext);

#endif

//--- ChibiStdlib.swift
precedencegroup AssignmentPrecedence {
  assignment: true
  associativity: right
}

public typealias Void = ()

@frozen
public struct OpaquePointer {
  @usableFromInline
  internal var _rawValue: Builtin.RawPointer

  @usableFromInline @_transparent
  internal init(_ v: Builtin.RawPointer) {
    self._rawValue = v
  }
}
