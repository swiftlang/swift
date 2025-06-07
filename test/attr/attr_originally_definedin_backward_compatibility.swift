// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=ios
// UNSUPPORTED: DARWIN_SIMULATOR=ios
// rdar://problem/65399527
// UNSUPPORTED: OS=ios
//
// RUN: %empty-directory(%t)
//
// -----------------------------------------------------------------------------
// --- Prepare SDK (.swiftmodule).
// RUN: %empty-directory(%t/SDK)
//

// --- Build original high level framework.
// RUN: mkdir -p %t/SDK/Frameworks/HighLevel.framework/Modules/HighLevel.swiftmodule
// RUN: %target-build-swift-dylib(%t/SDK/Frameworks/HighLevel.framework/HighLevel) -module-name HighLevel -emit-module \
// RUN:		-emit-module-path %t/SDK/Frameworks/HighLevel.framework/Modules/HighLevel.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     %S/Inputs/SymbolMove/HighlevelOriginal.swift -Xlinker -install_name -Xlinker @rpath/HighLevel.framework/HighLevel -enable-library-evolution

// --- Build an executable using the original high level framework
// RUN: %target-build-swift -emit-executable %s -g -o %t/HighlevelRunner -F %t/SDK/Frameworks/ -framework HighLevel \
// RUN: 	%target-rpath(@executable_path/SDK/Frameworks)

// --- Run the executable
// RUN: %target-codesign %t/SDK/Frameworks/HighLevel.framework/HighLevel
// RUN: %target-codesign %t/HighlevelRunner
// RUN: %target-run %t/HighlevelRunner %t/SDK/Frameworks/HighLevel.framework/HighLevel | %FileCheck %s -check-prefix=BEFORE_MOVE

// --- Build low level framework.
// RUN: mkdir -p %t/SDK/Frameworks/LowLevel.framework/Modules/LowLevel.swiftmodule
// RUN: %target-build-swift-dylib(%t/SDK/Frameworks/LowLevel.framework/LowLevel) -module-name LowLevel -emit-module \
// RUN:     -emit-module-path %t/SDK/Frameworks/LowLevel.framework/Modules/LowLevel.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     %S/Inputs/SymbolMove/LowLevel.swift -Xlinker -install_name -Xlinker @rpath/LowLevel.framework/LowLevel -enable-library-evolution \
// RUN:     -Xfrontend -define-availability -Xfrontend "_iOS13Aligned:macOS 10.10, iOS 8.0"

// --- Build high level framework.
// RUN: mkdir -p %t/SDK/Frameworks/HighLevel.framework/Modules/HighLevel.swiftmodule
// RUN: %target-build-swift-dylib(%t/SDK/Frameworks/HighLevel.framework/HighLevel) -module-name HighLevel -emit-module \
// RUN:		-emit-module-path %t/SDK/Frameworks/HighLevel.framework/Modules/HighLevel.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     %S/Inputs/SymbolMove/HighLevel.swift -F %t/SDK/Frameworks -Xlinker -reexport_framework -Xlinker LowLevel -enable-library-evolution

// --- Run the executable
// RUN: %target-codesign %t/SDK/Frameworks/HighLevel.framework/HighLevel
// RUN: %target-codesign %t/SDK/Frameworks/LowLevel.framework/LowLevel
// RUN: %target-codesign %t/HighlevelRunner
// RUN: %target-run %t/HighlevelRunner %t/SDK/Frameworks/HighLevel.framework/HighLevel %t/SDK/Frameworks/LowLevel.framework/LowLevel | %FileCheck %s -check-prefix=AFTER_MOVE

import HighLevel

printMessage()
printMessageMoved()

// BEFORE_MOVE: Hello from HighLevel
// BEFORE_MOVE: Hello from HighLevel
// AFTER_MOVE: Hello from LowLevel
// AFTER_MOVE: Hello from LowLevel

let e = Entity()
print(e.location())
// BEFORE_MOVE: Entity from HighLevel
// AFTER_MOVE: Entity from LowLevel

print(CandyBox(Candy()).ItemKind)
// BEFORE_MOVE: candy
// AFTER_MOVE: candy

print(CandyBox(Candy()).shape())
// BEFORE_MOVE: square
// AFTER_MOVE: round

print(LanguageKind.Cpp.rawValue)
// BEFORE_MOVE: -1
// AFTER_MOVE: 1

print("\(Vehicle().currentSpeed)")
// BEFORE_MOVE: -40
// AFTER_MOVE: 40

class Bicycle: Vehicle {}
let bicycle = Bicycle()
bicycle.currentSpeed = 15.0
print("\(bicycle.currentSpeed)")

// BEFORE_MOVE: 15.0
// AFTER_MOVE: 15.0

funcMacro()
// BEFORE_MOVE: Macro from HighLevel
// AFTER_MOVE: Macro from LowLevel
