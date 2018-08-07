import Foundation

public struct CommandLineArguments {
  public struct MissingArgumentError: LocalizedError {
    let argName: String

    public var errorDescription: String? {
      return "Missing required argument: \(argName)"
    }
  }
  public struct UnkeyedArgumentError: LocalizedError {
    let argName: String

    public var errorDescription: String? {
      return "Unexpectedly found command line argument \(argName) without a key"
    }
  }

  private let args: [String: String]

  public static func parse<T: Sequence>(_ args: T) throws -> CommandLineArguments
    where T.Element == String {
      var parsedArgs: [String: String] = [:]
      var currentKey: String? = nil
      for arg in args {
        if arg.hasPrefix("-") {
          // Parse a new key
          if let currentKey = currentKey {
            // The last key didn't have a value. Just add it with an empty string as
            // the value to the parsed args
            parsedArgs[currentKey] = ""
          }
          currentKey = arg
        } else {
          if let currentKey = currentKey {
            parsedArgs[currentKey] = arg
          } else {
            throw UnkeyedArgumentError(argName: arg)
          }
          currentKey = nil
        }
      }
      if let currentKey = currentKey {
        // The last key didn't have a value. Just add it with an empty string as
        // the value to the parsed args
        parsedArgs[currentKey] = ""
      }
      return CommandLineArguments(args: parsedArgs)
  }

  public subscript(key: String) -> String? {
    return args[key]
  }

  public func getRequired(_ key: String) throws -> String {
    if let value = args[key] {
      return value
    } else {
      throw MissingArgumentError(argName: key)
    }
  }

  public func has(_ key: String) -> Bool {
    return args[key] != nil
  }
}
