// RUN: %empty-directory(%t.relative_sdk_path)
// RUN: %empty-directory(%t.mcp)

// RUN: cp -R %S/Inputs/stdlib_rebuild %t.relative_sdk_path/
// RUN: cd %t.relative_sdk_path

// RUN: %target-swift-frontend(mock-sdk: -sdk stdlib_rebuild -module-cache-path %t.mcp) -target arm64-apple-macosx15.0 -resource-dir /dev/null -o %t.relative_sdk_path -emit-object %s

// REQUIRES: OS=macosx

import OtherModule

func fn(_: Int) {
    let _ = OtherStruct()
}
