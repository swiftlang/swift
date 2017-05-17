import Foundation
import StdlibUnittest

public func expectRoundTripEquality<T : Codable>(of value: T, encode: (T) throws -> Data, decode: (Data) throws -> T) where T : Equatable {
    let data: Data
    do {
        data = try encode(value)
    } catch {
        fatalError("Unable to encode \(T.self) <\(value)>: \(error)")
    }

    let decoded: T
    do {
        decoded = try decode(data)
    } catch {
        fatalError("Unable to decode \(T.self) <\(value)>: \(error)")
    }

    expectEqual(value, decoded, "Decoded \(T.self) <\(decoded)> not equal to original <\(value)>")
}

public func expectRoundTripEqualityThroughJSON<T : Codable>(for value: T) where T : Equatable {
    let encode = { (_ value: T) throws -> Data in
        return try JSONEncoder().encode(value)
    }

    let decode = { (_ data: Data) throws -> T in
        return try JSONDecoder().decode(T.self, from: data)
    }

    expectRoundTripEquality(of: value, encode: encode, decode: decode)
}

public func expectRoundTripEqualityThroughPlist<T : Codable>(for value: T) where T : Equatable {
    let encode = { (_ value: T) throws -> Data in
        return try PropertyListEncoder().encode(value)
    }

    let decode = { (_ data: Data) throws -> T in
        return try PropertyListDecoder().decode(T.self, from: data)
    }

    expectRoundTripEquality(of: value, encode: encode, decode: decode)
}
