// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -constraint-checker -parse -verify -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | grep blocks.pcm

import blocks

var someNSString : NSString
func useNSString(s:NSString) {}

dispatch_async(dispatch_get_current_queue(), func() { })
someNSString.enumerateLinesUsingBlock(func(s:NSString) { })
someNSString.enumerateLinesUsingBlock(func(s) { })
someNSString.enumerateLinesUsingBlock({ useNSString($0) })
