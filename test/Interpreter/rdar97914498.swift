// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -Xfrontend -disable-availability-checking -O -Xllvm -sil-opt-pass-count=0 -Xfrontend -disable-llvm-optzns %s -o %t/out
// RUN: %target-codesign %t/out
// RUN: %target-run %t/out

// This is a regression test that ensures that empty payloads in multi
// payload enums are properly handled.

import Foundation

@inline(never)
func crash() {
    let testURL = URL(string: "https://www.google.com")!
    let r = Resource<()>(url: testURL, method: .get)
    print(r.url)
}

enum HTTPMethod<Payload> {
    case get
    case post(Payload)
    case patch(Payload)
}

struct Resource<Payload> {
    let url: URL
    let method: HTTPMethod<Payload>
}

crash()
