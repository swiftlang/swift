// RUN: %target-typecheck-verify-swift -Xcc -DINCORRECT -I %S%{fs-sep}Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}lifetime-operation-methods.h -cxx-interoperability-mode=upcoming-swift -disable-availability-checking

import LifetimeOperationMethods

let _ = StaticRetainRelease(123)
let _ = DerivedStaticRetainRelease(123, 456)
