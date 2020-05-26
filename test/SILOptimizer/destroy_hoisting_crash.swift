// RUN: %target-swift-frontend -O %s -emit-sil -o /dev/null

public struct S {
  let args: [Substring]
  let arg: Substring

  enum Error: Swift.Error {
    case Case
  }

  public init(arg: String) throws {
    args = arg.split(separator: "\n")
    guard args.count > 0 else { throw Error.Case }

    let parts = args[0].split(separator: " ")
    guard parts.count > 2 else { throw Error.Case }

    self.arg = parts[1]
  }
}

