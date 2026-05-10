// RUN: %target-swift-frontend -I %S/Inputs -cxx-interoperability-mode=swift-5.9 -emit-ir %s -Xcc -fignore-exceptions | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import CxxStdlib

// CHECK: @"\01L_selector(UTF8String)"
// CHECK: @objc_msgSend
// CHECK: call swiftcc void @"$sSo3stdO3__1O0088basic_stringCCharstd__1char_traitsCCharstd__1allocatorCChar_cyHBywaEDexaCidvdFCgAayGjzaaV9CxxStdlibEyAFSPys4Int8VGSgcfC"

let ObjCStr: NSString = "hello"
let CxxStr = std.string(ObjCStr.utf8String) // Should not crash here
