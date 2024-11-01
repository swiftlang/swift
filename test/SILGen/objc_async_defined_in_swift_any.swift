// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -target %target-swift-5.1-abi-triple -verify %s
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
