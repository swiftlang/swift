// RUN: %target-typecheck-verify-swift

enum EnumWithExcludedElement : Codable {
    case x
    case y

    enum CodingKeys: CodingKey {
        case x
    }
}
