// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/51320

public protocol LocalizedError : Error {
    var errorDescription: String? { get }
}

public enum AFError: Error {}
extension AFError {
    public var isInvalidURLError: Bool {

extension AFError: LocalizedError {
    public var errorDescription: String? {
      return ""
