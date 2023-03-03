// RUN: %empty-directory(%t)

// >> first try when no library evolution is specified
// RUN: %target-swift-frontend -DSYNTHESIZE_ACCESSORS -enable-experimental-move-only -emit-module -o %t/Hello.swiftmodule %S/Inputs/moveonly_api.swift
// RUN: %target-swift-frontend -emit-sil -sil-verify-all -enable-experimental-move-only -I %t %s > /dev/null

// >> now again with library evolution; we expect the same result.
// FIXME: move checker doesn't like it when you specify library evolution
// RUN: %target-swift-frontend -DSYNTHESIZE_ACCESSORS -enable-library-evolution -enable-experimental-move-only -emit-module -o %t/Hello.swiftmodule %S/Inputs/moveonly_api.swift
// RUN: %target-swift-frontend -emit-sil -sil-verify-all -enable-experimental-move-only -I %t %s > /dev/null

// FIXME: ideally this would also try executing the program rather than just generating SIL

// FIXME: make this test work when we're not synthesizing the accessors

// rdar://106164128
// XFAIL: *

import Hello

func simpleTest() {
  let handle = FileHandle()
  let msg = handle.file.fd.message()
  print(msg)
}
