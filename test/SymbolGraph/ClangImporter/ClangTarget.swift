// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Output)

// Build the explicit Clang modules for a different target than the Swift
// invocation. The extractor must use -clang-target to load these PCMs.
// RUN: %target-swift-emit-pcm -target %target-cpu-apple-macosx12.0 -module-name ClangTarget \
// RUN:   %S/Inputs/ClangTarget/module.modulemap -o %t/ClangTarget.pcm -Xcc -Xclang -Xcc -fbuiltin-headers-in-system-modules
// RUN: %target-swift-emit-pcm -target %target-cpu-apple-macosx12.0 -module-name SwiftShims \
// RUN:   %swift-lib-dir/swift/shims/module.modulemap -o %t/SwiftShims.pcm -Xcc -Xclang -Xcc -fbuiltin-headers-in-system-modules
// RUN: %target-swift-symbolgraph-extract -sdk %sdk -target %target-cpu-apple-macosx11.0 -clang-target %target-cpu-apple-macosx12.0 \
// RUN:   -module-name ClangTarget -I %S/Inputs/ClangTarget \
// RUN:   -Xcc -fmodule-file=ClangTarget=%t/ClangTarget.pcm -Xcc -fmodule-map-file=%S/Inputs/ClangTarget/module.modulemap \
// RUN:   -Xcc -fmodule-file=SwiftShims=%t/SwiftShims.pcm -Xcc -fmodule-map-file=%swift-lib-dir/swift/shims/module.modulemap \
// RUN:   -Xcc -Xclang -Xcc -fbuiltin-headers-in-system-modules -Xcc -fno-implicit-module-maps -Xcc -fno-implicit-modules \
// RUN:   -pretty-print -output-dir %t/Output
// RUN: %FileCheck %s --input-file %t/Output/ClangTarget.symbols.json

// CHECK: "title": "ClangTargetValue"
