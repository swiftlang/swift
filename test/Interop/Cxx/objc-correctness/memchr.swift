// RUN: %target-swift-frontend -cxx-interoperability-mode=default -typecheck -verify -I %S/Inputs %s

// REQUIRES: objc_interop
// REQUIRES: VENDOR=apple

import Darwin

func test_memchr() {
    var src = "hello"
    var _ = src.withUTF8 { srcBuf in
        Darwin.memchr(srcBuf.baseAddress!, 137, src.count)
    }
}
