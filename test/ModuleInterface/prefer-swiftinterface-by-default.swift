/// The blocklist can enable loading distributed swiftinterfaces by default.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines
// REQUIRES: VENDOR=apple

/// Setup the SDK and local modules.
//// SDK module in S/L/F.
// RUN: %target-swift-frontend -emit-module -module-name SDKModule %t/Empty.swift \
// RUN:   -enable-library-evolution -swift-version 5 -parse-stdlib \
// RUN:   -emit-module-path %t/sdk/System/Library/Frameworks/SDKModule.framework/Modules/SDKModule.swiftmodule/%target-swiftmodule-name \
// RUN:   -emit-module-interface-path %t/sdk/System/Library/Frameworks/SDKModule.framework/Modules/SDKModule.swiftmodule/%target-swiftinterface-name
// RUN: %target-swift-typecheck-module-from-interface(%t/sdk/System/Library/Frameworks/SDKModule.framework/Modules/SDKModule.swiftmodule/%target-swiftinterface-name) -module-name SDKModule

//// SDK module not in S/L/F.
// RUN: %target-swift-frontend -emit-module -module-name SDKModuleAtUnusualPath %t/Empty.swift \
// RUN:   -enable-library-evolution -swift-version 5 -parse-stdlib \
// RUN:   -emit-module-path %t/sdk/System/Library/OtherFrameworks/SDKModuleAtUnusualPath.framework/Modules/SDKModuleAtUnusualPath.swiftmodule/%target-swiftmodule-name \
// RUN:   -emit-module-interface-path %t/sdk/System/Library/OtherFrameworks/SDKModuleAtUnusualPath.framework/Modules/SDKModuleAtUnusualPath.swiftmodule/%target-swiftinterface-name
// RUN: %target-swift-typecheck-module-from-interface(%t/sdk/System/Library/OtherFrameworks/SDKModuleAtUnusualPath.framework/Modules/SDKModuleAtUnusualPath.swiftmodule/%target-swiftinterface-name) -module-name SDKModuleAtUnusualPath

//// Local module.
// RUN: %target-swift-frontend -emit-module -module-name LocalModule %t/Empty.swift \
// RUN:   -enable-library-evolution -swift-version 5 -parse-stdlib \
// RUN:   -emit-module-path %t/not_sdk/LocalModule.swiftmodule \
// RUN:   -emit-module-interface-path %t/not_sdk/LocalModule.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/not_sdk/LocalModule.swiftinterface) -module-name LocalModule

//// Host resource-dir module.
// RUN: %target-swift-frontend -emit-module -module-name HostResourceDirModule %t/Empty.swift \
// RUN:   -enable-library-evolution -swift-version 5 -parse-stdlib \
// RUN:   -emit-module-path %t/res/host/HostResourceDirModule.swiftmodule \
// RUN:   -emit-module-interface-path %t/res/host/HostResourceDirModule.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/res/host/HostResourceDirModule.swiftinterface) -module-name HostResourceDirModule

//// Blocklisted module.
// RUN: %target-swift-frontend -emit-module -module-name BlocklistedModule %t/Empty.swift \
// RUN:   -enable-library-evolution -swift-version 5 -parse-stdlib \
// RUN:   -emit-module-path %t/sdk/System/Library/Frameworks/BlocklistedModule.framework/Modules/BlocklistedModule.swiftmodule/%target-swiftmodule-name \
// RUN:   -emit-module-interface-path %t/sdk/System/Library/Frameworks/BlocklistedModule.framework/Modules/BlocklistedModule.swiftmodule/%target-swiftinterface-name
// RUN: %target-swift-typecheck-module-from-interface(%t/sdk/System/Library/Frameworks/BlocklistedModule.framework/Modules/BlocklistedModule.swiftmodule/%target-swiftinterface-name) -module-name BlocklistedModule

/// Build a client preferring swiftinterfaces.
// RUN: %target-swift-frontend -typecheck -module-name Main %t/Client_SwiftinterfacesByDefault.swift \
// RUN:   -parse-stdlib -module-cache-path %t/cache \
// RUN:   -blocklist-file %t/blocklistEnabled.yml \
// RUN:   -sdk %t/sdk -I %t/not_sdk -resource-dir %t/res -I %t/res/host/ \
// RUN:   -F %t/sdk/System/Library/OtherFrameworks/ \
// RUN:   -Rmodule-interface-rebuild -verify

/// Build a client preferring swiftmodules.
// RUN: %empty-directory(%t/cache)
// RUN: %target-swift-frontend -typecheck -module-name Main %t/Client_SwiftmodulesByDefault.swift \
// RUN:   -parse-stdlib -module-cache-path %t/cache \
// RUN:   -blocklist-file %t/blocklistDisabled.yml \
// RUN:   -sdk %t/sdk -I %t/not_sdk -resource-dir %t/res -I %t/res/host/ \
// RUN:   -F %t/sdk/System/Library/OtherFrameworks/ \
// RUN:   -Rmodule-interface-rebuild -verify

//--- Empty.swift

//--- Client_SwiftinterfacesByDefault.swift
/// New behavior
import SDKModule // expected-remark {{rebuilding module 'SDKModule' from interface}}
// expected-note @-1 {{was ignored because the default is to load from module interfaces}}
import SDKModuleAtUnusualPath // expected-remark {{rebuilding module 'SDKModuleAtUnusualPath' from interface}}
// expected-note @-1 {{was ignored because the default is to load from module interfaces}}
import HostResourceDirModule // expected-remark {{rebuilding module 'HostResourceDirModule' from interface}}
// expected-note @-1 {{was ignored because the default is to load from module interfaces}}
import LocalModule // expected-note {{not defaulting to module interface because it is a local module, preferring binary module at}}
import BlocklistedModule // expected-note {{not defaulting to module interface because it was blocklisted, preferring binary module at}}

//--- Client_SwiftmodulesByDefault.swift
/// Old behavior
import SDKModule // expected-remark {{rebuilding module 'SDKModule' from interface}}
// expected-note @-1 {{was ignored because it belongs to a framework in the SDK}}
import SDKModuleAtUnusualPath
import HostResourceDirModule // expected-remark {{rebuilding module 'HostResourceDirModule' from interface}}
// expected-note @-1 {{was ignored because it's a compiler host module}}
import LocalModule
import BlocklistedModule

//--- blocklistDisabled.yml
---
ShouldUseBinaryModule:
  ModuleName:
    - BlocklistedModule

//--- blocklistEnabled.yml
---
ShouldUseBinaryModule:
  ModuleName:
    - Swift_UseSwiftinterfaceByDefault
    - BlocklistedModule
