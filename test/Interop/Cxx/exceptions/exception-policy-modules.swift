// DEFINE: %{strict} = -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -enable-experimental-feature CxxExceptionBridgingStrict
// DEFINE: %{annotated} = -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Strict mode requires C++ interoperability and CxxExceptionBridging.
// RUN: not %target-swift-frontend -typecheck %t/Library.swift -enable-experimental-feature CxxExceptionBridgingStrict 2>&1 | %FileCheck %s --check-prefix=REQUIRES-BOTH
// RUN: not %target-swift-frontend -typecheck %t/Library.swift -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridgingStrict 2>&1 | %FileCheck %s --check-prefix=REQUIRES-FEATURE
// REQUIRES-BOTH: experimental feature 'CxxExceptionBridgingStrict' requires C++ interoperability
// REQUIRES-BOTH: experimental feature 'CxxExceptionBridgingStrict' requires '-enable-experimental-feature CxxExceptionBridging'
// REQUIRES-FEATURE-NOT: requires C++ interoperability
// REQUIRES-FEATURE: experimental feature 'CxxExceptionBridgingStrict' requires '-enable-experimental-feature CxxExceptionBridging'

// Only a module built in strict mode records it, even if the producer disabled
// the C++ interoperability requirement. A module built with C++
// interoperability can only be used in the mode it was built in.
// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name Annotated -o %t/Annotated.swiftmodule -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name Strict -o %t/Strict.swiftmodule %{strict}
// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name BypassedStrict -o %t/BypassedStrict.swiftmodule -disable-cxx-interop-requirement-at-import %{strict}
// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name PureSwift -o %t/PureSwift.swiftmodule
// RUN: %llvm-bcanalyzer --dump %t/Annotated.swiftmodule | %FileCheck %s --check-prefix=NOT-RECORDED
// RUN: %llvm-bcanalyzer --dump %t/Strict.swiftmodule | %FileCheck %s --check-prefix=RECORDED
// RUN: %llvm-bcanalyzer --dump %t/BypassedStrict.swiftmodule | %FileCheck %s --check-prefix=RECORDED
// RUN: %target-swift-frontend -typecheck %t/annotated.swift -I %t -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -typecheck %t/annotated.swift -I %t %{annotated}
// RUN: %target-swift-frontend -typecheck %t/strict.swift -I %t %{strict}
// RUN: not %target-swift-frontend -typecheck %t/annotated.swift -I %t %{strict} 2>&1 | %FileCheck %s --check-prefix=ANNOTATED-IN-STRICT
// RUN: not %target-swift-frontend -typecheck %t/strict.swift -I %t %{annotated} 2>&1 | %FileCheck %s --check-prefix=STRICT-IN-ANNOTATED
// RUN: not %target-swift-frontend -typecheck %t/bypassed.swift -I %t %{annotated} 2>&1 | %FileCheck %s --check-prefix=BYPASSED
// RUN: %target-swift-frontend -typecheck %t/pure.swift -I %t %{strict}
// RUN: %target-swift-frontend -typecheck %t/pure.swift -I %t %{annotated}
// NOT-RECORDED-NOT: CXX_EXCEPTION_BRIDGING_STRICT
// RECORDED: CXX_EXCEPTION_BRIDGING_STRICT
// ANNOTATED-IN-STRICT: module 'Annotated' was built without experimental feature 'CxxExceptionBridgingStrict', but the current compilation enables it
// STRICT-IN-ANNOTATED: module 'Strict' was built with experimental feature 'CxxExceptionBridgingStrict', but the current compilation does not enable it
// BYPASSED: module 'BypassedStrict' was built with experimental feature 'CxxExceptionBridgingStrict', but the current compilation does not enable it

// A strict interface records the feature, so an annotated consumer can't use it.
// RUN: %empty-directory(%t/interfaces)
// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name Strict -o %t/interfaces/Strict.swiftmodule -emit-module-interface-path %t/interfaces/Strict.swiftinterface -enable-library-evolution -swift-version 6 %{strict}
// RUN: %FileCheck %s --check-prefix=INTERFACE < %t/interfaces/Strict.swiftinterface
// RUN: rm %t/interfaces/Strict.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/strict.swift -I %t/interfaces -module-cache-path %t/cache %{strict}
// RUN: not %target-swift-frontend -typecheck %t/strict.swift -I %t/interfaces -module-cache-path %t/cache %{annotated} 2>&1 | %FileCheck %s --check-prefix=STRICT-IN-ANNOTATED
// INTERFACE: // swift-module-flags: {{.*}}-enable-experimental-feature CxxExceptionBridgingStrict

// A strict consumer rebuilds other interfaces in strict mode: a C++
// interoperability interface built in annotated mode, a pure Swift interface,
// and a pure Swift interface that predates -formal-cxx-interoperability-mode.
// An annotated consumer rebuilds the same interfaces in annotated mode.
// RUN: %empty-directory(%t/rebuilt)
// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name Annotated -o %t/rebuilt/Annotated.swiftmodule -emit-module-interface-path %t/rebuilt/Annotated.swiftinterface -enable-library-evolution -swift-version 6 -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name PureSwift -o %t/rebuilt/PureSwift.swiftmodule -emit-module-interface-path %t/rebuilt/PureSwift.swiftinterface -enable-library-evolution -swift-version 6
// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name OldPureSwift -o %t/rebuilt/OldPureSwift.swiftmodule -emit-module-interface-path %t/rebuilt/OldPureSwift.swiftinterface -enable-library-evolution -swift-version 6
// RUN: rm %t/rebuilt/*.swiftmodule
// RUN: sed -i.bak -e 's/-formal-cxx-interoperability-mode=off//' %t/rebuilt/OldPureSwift.swiftinterface
// RUN: %FileCheck %s --check-prefix=OLD-INTERFACE < %t/rebuilt/OldPureSwift.swiftinterface
// RUN: %target-swift-frontend -typecheck %t/annotated.swift -I %t/rebuilt -module-cache-path %t/strict-cache %{strict}
// RUN: %target-swift-frontend -typecheck %t/pure.swift -I %t/rebuilt -module-cache-path %t/strict-cache %{strict}
// RUN: %target-swift-frontend -typecheck %t/old-pure.swift -I %t/rebuilt -module-cache-path %t/strict-cache %{strict}
// RUN: %target-swift-frontend -typecheck %t/annotated.swift -I %t/rebuilt -module-cache-path %t/strict-cache %{annotated}
// RUN: %target-swift-frontend -typecheck %t/pure.swift -I %t/rebuilt -module-cache-path %t/strict-cache %{annotated}
// RUN: %target-swift-frontend -typecheck %t/old-pure.swift -I %t/rebuilt -module-cache-path %t/strict-cache %{annotated}
// RUN: %target-swift-frontend -typecheck %t/annotated.swift -I %t/rebuilt -module-cache-path %t/annotated-cache -cxx-interoperability-mode=default
// OLD-INTERFACE-NOT: -formal-cxx-interoperability-mode

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: swift_feature_CxxExceptionBridgingStrict
// UNSUPPORTED: OS=windows-msvc

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
// The Cxx module works in both modes.
@_spi(CxxExceptionBridging) import Cxx
let result = value()
func message(_ error: CxxException) -> String { error.message }

//--- old-pure.swift
import OldPureSwift
let result = value()
