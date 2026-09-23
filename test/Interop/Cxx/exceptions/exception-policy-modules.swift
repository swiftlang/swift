// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name Annotated -o %t/Annotated.swiftmodule -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name Strict -o %t/Strict.swiftmodule -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging
// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name BypassedStrict -o %t/BypassedStrict.swiftmodule -disable-cxx-interop-requirement-at-import -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging
// RUN: not %target-swift-frontend -typecheck %t/bypassed.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging 2>&1 | %FileCheck %s --check-prefix=BYPASSED
// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name PureSwift -o %t/PureSwift.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/annotated.swift -I %t -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -typecheck %t/strict.swift -I %t -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging
// RUN: not %target-swift-frontend -typecheck %t/annotated.swift -I %t -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging 2>&1 | %FileCheck %s --check-prefix=ANNOTATED-IN-STRICT
// RUN: not %target-swift-frontend -typecheck %t/strict.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging 2>&1 | %FileCheck %s --check-prefix=STRICT-IN-ANNOTATED
// RUN: %target-swift-frontend -typecheck %t/pure.swift -I %t -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging
// RUN: %empty-directory(%t/annotated-interface)
// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name Annotated -o %t/annotated-interface/Annotated.swiftmodule -emit-module-interface-path %t/annotated-interface/Annotated.swiftinterface -enable-library-evolution -swift-version 6 -cxx-interoperability-mode=default
// RUN: rm %t/annotated-interface/Annotated.swiftmodule
// RUN: not %target-swift-frontend -typecheck %t/annotated.swift -I %t/annotated-interface -module-cache-path %t/annotated-cache -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging 2>&1 | %FileCheck %s --check-prefix=ANNOTATED-IN-STRICT
// RUN: %empty-directory(%t/interfaces)
// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name Strict -o %t/interfaces/Strict.swiftmodule -emit-module-interface-path %t/interfaces/Strict.swiftinterface -enable-library-evolution -swift-version 6 -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging
// RUN: %FileCheck %s --check-prefix=INTERFACE < %t/interfaces/Strict.swiftinterface
// RUN: rm %t/interfaces/Strict.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/strict.swift -I %t/interfaces -module-cache-path %t/cache -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging
// RUN: not %target-swift-frontend -typecheck %t/strict.swift -I %t/interfaces -module-cache-path %t/cache -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging 2>&1 | %FileCheck %s --check-prefix=STRICT-IN-ANNOTATED
// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name PureSwift -o %t/interfaces/PureSwift.swiftmodule -emit-module-interface-path %t/interfaces/PureSwift.swiftinterface -enable-library-evolution -swift-version 6
// RUN: rm %t/interfaces/PureSwift.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/pure.swift -I %t/interfaces -module-cache-path %t/cache -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging

// A pure Swift interface rebuilt under a C++ consumer remains policy independent.
// RUN: %empty-directory(%t/rebuilt-pure)
// RUN: %target-swift-frontend -compile-module-from-interface %t/interfaces/PureSwift.swiftinterface -o %t/rebuilt-pure/PureSwift.swiftmodule -module-name PureSwift -formal-cxx-interoperability-mode=off -cxx-interoperability-mode=default -cxx-exception-mode=strict -enable-experimental-feature CxxExceptionBridging
// RUN: %target-swift-frontend -typecheck %t/pure-no-cxx.swift -I %t/rebuilt-pure
// RUN: %target-swift-frontend -compile-module-from-interface %t/interfaces/PureSwift.swiftinterface -o %t/rebuilt-pure/PureSwift.swiftmodule -module-name PureSwift -formal-cxx-interoperability-mode=off -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging
// RUN: %target-swift-frontend -typecheck %t/pure-no-cxx.swift -I %t/rebuilt-pure

// REQUIRES: swift_feature_CxxExceptionBridging
// UNSUPPORTED: OS=windows-msvc

// ANNOTATED-IN-STRICT: module 'Annotated' was built with C++ exception mode 'annotated', but current compilation uses 'strict'
// STRICT-IN-ANNOTATED: module 'Strict' was built with C++ exception mode 'strict', but current compilation uses 'annotated'
// BYPASSED: module 'BypassedStrict' was built with C++ exception mode 'strict', but current compilation uses 'annotated'
// INTERFACE: // swift-module-flags: {{.*}}-cxx-exception-mode=strict

//--- Library.swift
public func value() -> Int { 42 }

//--- annotated.swift
import Annotated
let result = value()

//--- strict.swift
import Strict
let result = value()

//--- bypassed.swift
import BypassedStrict
let result = value()

//--- pure.swift
import PureSwift
import Cxx
let result = value()
let error = CxxException(message: "failure")

//--- pure-no-cxx.swift
import PureSwift
let result = value()
