// Make sure that Swift's search paths order the same as the corresponding clang flags
// In particular, they should all come before the default usr/local/include

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-clang -isysroot %t/sdk -I%t/sdk/custom/include -fmodules -fmodules-cache-path=%t/mcp1 -fsyntax-only %t/SearchPathOrder.m
// RUN: %target-clang -isysroot %t/sdk -isystem %t/sdk/custom/include -fmodules -fmodules-cache-path=%t/mcp2 -fsyntax-only %t/SearchPathOrder.m
// RUN: %target-clang -isysroot %t/sdk -F%t/sdk/custom/Frameworks -fmodules -fmodules-cache-path=%t/mcp3 -fsyntax-only %t/SearchPathOrder.m
// RUN: %target-clang -isysroot %t/sdk -iframework %t/sdk/custom/Frameworks -fmodules -fmodules-cache-path=%t/mcp4 -fsyntax-only %t/SearchPathOrder.m

// RUN: %target-swift-frontend -sdk %t/sdk -typecheck -I %t/sdk/custom/include -module-cache-path %t/mcp5 %t/SearchPathOrder.swift
// RUN: %target-swift-frontend -sdk %t/sdk -typecheck -Isystem %t/sdk/custom/include -module-cache-path %t/mcp6 %t/SearchPathOrder.swift
// RUN: %target-swift-frontend -sdk %t/sdk -typecheck -F %t/sdk/custom/Frameworks -module-cache-path %t/mcp7 %t/SearchPathOrder.swift
// RUN: %target-swift-frontend -sdk %t/sdk -typecheck -Fsystem %t/sdk/custom/Frameworks -module-cache-path %t/mcp8 %t/SearchPathOrder.swift

// If both clang and Swift error then it's a problem with this test.
// If only Swift errors then it's a problem with the clang importer search paths code.

//--- sdk/usr/local/include/module.modulemap
module Module {
  module bomb { header "bomb.h" }
}

//--- sdk/usr/local/include/bomb.h
#error "bomb"


//--- sdk/custom/include/module.modulemap
module Module {
  module good { header "good.h" }
}

//--- sdk/custom/include/good.h

//--- sdk/custom/Frameworks/Module.framework/Modules/module.modulemap
framework module Module {
  module good { header "good.h" }
}

//--- sdk/custom/Frameworks/Module.framework/Headers/good.h





//--- SearchPathOrder.m
@import Module;

//--- SearchPathOrder.swift
import Module
