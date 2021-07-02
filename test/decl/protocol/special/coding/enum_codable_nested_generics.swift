// RUN: %target-typecheck-verify-swift

struct Container<Element> {
    let options: [Option]
}

extension Container {
    enum Option {
        case first (Element)
        case second (Element)
    }
}

extension Container: Codable where Element: Codable {}
extension Container.Option: Codable where Element: Codable {}