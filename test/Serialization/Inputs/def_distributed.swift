import _Distributed

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
distributed actor DA {
    public distributed func doSomethingDistributed() async -> Int {
        return 0
    }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension DA {
    public static func _remote_doSomethingDistributed() async throws -> Int {
        fatalError()
    }
}
