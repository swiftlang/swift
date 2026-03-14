/// ProductBuildVersion of the SDK is tracked as part of the module cache hash.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/sdk/System/Library/CoreServices/)
// RUN: split-file %s %t

/// Setup an "old" SDK.
// RUN: cp %t/SystemVersion.plist.old %t/sdk/System/Library/CoreServices/SystemVersion.plist

/// Build Lib against the old SDK.
// RUN: %target-swift-frontend -emit-module -sdk %t/sdk %t/Lib.swift \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface

/// Baseline check, we should read the adjacent swiftmodule.
// RUN: %target-swift-frontend -typecheck -verify -sdk %t/sdk -I %t \
// RUN:   %t/Client_NoRebuild.swift \
// RUN:   -Rmodule-interface-rebuild -module-cache-path %t/ModuleCache

/// Keep only the swiftinterface.
// RUN: rm %t/Lib.swiftmodule

/// Build client, which should trigger a build from swiftinterface.
// RUN: %target-swift-frontend -typecheck -verify -sdk %t/sdk -I %t \
// RUN:   %t/Client.swift \
// RUN:   -Rmodule-interface-rebuild -module-cache-path %t/ModuleCache

/// Update SDK.
// RUN: cp %t/SystemVersion.plist.new %t/sdk/System/Library/CoreServices/SystemVersion.plist

/// Build client, which should trigger a build from swiftinterface.
// RUN: %target-swift-frontend -typecheck -verify -sdk %t/sdk -I %t \
// RUN:   %t/Client.swift \
// RUN:   -Rmodule-interface-rebuild -module-cache-path %t/ModuleCache

/// Baseline check, we should reused the newly cached swiftmodule.
// RUN: %target-swift-frontend -typecheck -verify -sdk %t/sdk -I %t \
// RUN:   %t/Client_NoRebuild.swift \
// RUN:   -Rmodule-interface-rebuild -module-cache-path %t/ModuleCache

//--- SystemVersion.plist.old
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>ProductBuildVersion</key>
	<string>10A100</string>
</dict>
</plist>

//--- SystemVersion.plist.new
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>ProductBuildVersion</key>
	<string>10A200</string>
</dict>
</plist>

//--- Lib.swift
public func foo() {}

//--- Client.swift
import Lib // expected-remark {{rebuilding module 'Lib' from interface}}

//--- Client_NoRebuild.swift
import Lib

