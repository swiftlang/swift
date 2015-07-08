// RUN: %target-parse-verify-swift

protocol SomeProtocol {
	typealias T
}

extension SomeProtocol where T == Optional<T> { } // expected-error{{same-type constraint '`Self`.T' == 'Optional<`Self`.T>' is recursive}}
