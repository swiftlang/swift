// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %empty-directory(%t/ModuleCache)

// Disabled COM interop builds one cache entry and then reuses it.
// RUN: %target-swift-frontend -parse-stdlib -typecheck \
// RUN:   %t/RebuildDisabled.swift -I %t -module-cache-path %t/ModuleCache \
// RUN:   -Rmodule-interface-rebuild -verify
// RUN: %target-swift-frontend -parse-stdlib -typecheck \
// RUN:   %t/UseDisabled.swift -I %t -module-cache-path %t/ModuleCache \
// RUN:   -Rmodule-interface-rebuild -verify
// RUN: %find_files %t/ModuleCache 'Lib-*.swiftmodule' \
// RUN:   | %llvm_obj_root/bin/count 1

// The Microsoft model gets a distinct cache entry and then reuses it.
// RUN: %target-swift-frontend -parse-stdlib -typecheck \
// RUN:   %t/RebuildMicrosoft.swift -I %t -module-cache-path %t/ModuleCache \
// RUN:   -Rmodule-interface-rebuild -verify \
// RUN:   -enable-experimental-com-interop -com-interop-model=microsoft \
// RUN:   -disable-implicit-com-module-import
// RUN: %target-swift-frontend -parse-stdlib -typecheck \
// RUN:   %t/UseMicrosoft.swift -I %t -module-cache-path %t/ModuleCache \
// RUN:   -Rmodule-interface-rebuild -verify \
// RUN:   -enable-experimental-com-interop -com-interop-model=microsoft \
// RUN:   -disable-implicit-com-module-import
// RUN: %find_files %t/ModuleCache 'Lib-*.swiftmodule' \
// RUN:   | %llvm_obj_root/bin/count 2

// The CoreFoundation model gets a third cache entry and then reuses it.
// RUN: %target-swift-frontend -parse-stdlib -typecheck \
// RUN:   %t/RebuildCoreFoundation.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify \
// RUN:   -enable-experimental-com-interop -com-interop-model=corefoundation \
// RUN:   -disable-implicit-com-module-import
// RUN: %target-swift-frontend -parse-stdlib -typecheck \
// RUN:   %t/UseCoreFoundation.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify \
// RUN:   -enable-experimental-com-interop -com-interop-model=corefoundation \
// RUN:   -disable-implicit-com-module-import
// RUN: %find_files %t/ModuleCache 'Lib-*.swiftmodule' \
// RUN:   | %llvm_obj_root/bin/count 3

//--- Lib.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -parse-stdlib -module-name Lib

#if $_MicrosoftCOM
public enum Microsoft {}
#elseif $_CoreFoundationCOM
public enum CoreFoundation {}
#else
public enum Disabled {}
#endif

//--- RebuildDisabled.swift
import Lib // expected-remark {{rebuilding module 'Lib' from interface}}
func use(_: Disabled.Type) {}

//--- UseDisabled.swift
import Lib
func use(_: Disabled.Type) {}

//--- RebuildMicrosoft.swift
import Lib // expected-remark {{rebuilding module 'Lib' from interface}}
func use(_: Microsoft.Type) {}

//--- UseMicrosoft.swift
import Lib
func use(_: Microsoft.Type) {}

//--- RebuildCoreFoundation.swift
import Lib // expected-remark {{rebuilding module 'Lib' from interface}}
func use(_: CoreFoundation.Type) {}

//--- UseCoreFoundation.swift
import Lib
func use(_: CoreFoundation.Type) {}
