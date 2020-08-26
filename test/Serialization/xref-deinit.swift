// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sib %s -o %t/xref-deinit.sib -I%t -I %S/Inputs/objc-xref
// RUN: %target-swift-frontend -emit-sil %t/xref-deinit.sib -I%t -I %S/Inputs/objc-xref

// REQUIRES: objc_interop

import ObjCXRef

public class Object: MyObject {}
