enum ColorParsingError: Error {
    case unknown(String)
}

enum Color {
    static func hex(_ hex: borrowing String) throws -> Self {
        throw ColorParsingError.unknown(hex)
    }
}
