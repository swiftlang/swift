// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -disable-availability-checking -verify %s
// REQUIRES: concurrency
// REQUIRES: objc_interop

import Foundation 
@objc public protocol TAService {
    func removeKey() async -> Any
}

class FakeService : TAService {
    func removeKey() async -> Any {
        return ""
    }
}

class FakeClassHolder {
    var service : TAService = FakeService()

    func removeKey(_ key: String) async {
        _ = await self.service.removeKey()
    }
}
