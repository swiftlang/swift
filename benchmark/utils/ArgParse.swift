//===--- ArgParse.swift ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation

enum ArgumentError: Error {
  case missingValue(String)
  case invalidType(value: String, type: String, argument: String?)
  case unsupportedArgument(String)
}

extension ArgumentError: CustomStringConvertible {
  public var description: String {
    switch self {
    case let .missingValue(key):
      return "missing value for '\(key)'"
    case let .invalidType(value, type, argument):
      return (argument == nil)
        ? "'\(value)' is not a valid '\(type)'"
        : "'\(value)' is not a valid '\(type)' for '\(argument!)'"
    case let .unsupportedArgument(argument):
      return "unsupported argument '\(argument)'"
    }
  }
}

/// Returns the argument value converted to the type T using the parser.
/// If the parser cannot create the value of specified type, throw an invalid
/// type argument error.
func checked<T>(
  _ parse: (String) throws -> T?,
  _ value: String,
  argument: String? = nil
) throws -> T {
  if let t = try parse(value)  { return t }
  var type = "\(T.self)"
  if type.starts(with: "Optional<") {
      let s = type.index(after: type.index(of:"<")!)
      let e = type.index(before: type.endIndex) // ">"
      type = String(type[s..<e]) // strip Optional< >
  }
  throw ArgumentError.invalidType(
    value: value, type: type, argument: argument)
}

/// Parser that converts the program's command line arguments to typed values
/// according to the parser's configuration, storing them in the provided
/// instance of a value-holding type.
class ArgumentParser<U> {
    private var result: U
    private var validOptions: [String] {
      return arguments.compactMap { $0.name }
    }
    private var arguments: [Argument] = []
    private let progName = CommandLine.arguments[0]
    private var positionalArgs = [String]()
    private var optionalArgsMap = [String : String]()

    // Argument holds the name of the command line parameter and the value
    // processing closure used to convert it into given type and storing it
    // in the parsing result.
    struct Argument {
      let name: String?
      let apply: () throws -> ()
    }

    /// ArgumentParser is initialized with an instance of a type that holds
    /// the results of the parsing of the individual command line arguments.
    init(into result: U) {
      self.result = result
      self.arguments += [
        Argument(name: "--help", apply: printUsage)
      ]
    }

    private func printUsage() {
      guard let _ = self.optionalArgsMap["--help"] else { return }
      print("Valid options:")
      for v in validOptions {
        print("    \(v)")
      }
      exit(0)
    }

    /// Parses the command line arguments, returning the result filled with
    /// specified argument values or report errors and exit the program if
    /// the parsing fails.
    public func parse() -> U {
      do {
        try parseArgs()
        try arguments.forEach { try $0.apply() } // parse all arguments
        return result
      } catch let error as ArgumentError {
        fflush(stdout)
        fputs("error: \(error)\n", stderr)
        fflush(stderr)
        exit(1)
      } catch {
        fatalError("\(error)")
      }
    }

    /// Using CommandLine.arguments, parses the structure of optional and
    /// positional arguments of this program. Failure to parse arguments
    /// throws correspondding ArgumentError.
    ///
    /// We assume that optional switch args are of the form:
    ///
    /// --opt-name[=opt-value]
    /// -opt-name[=opt-value]
    ///
    /// with opt-name and opt-value not containing any '=' signs. Any
    /// other option passed in is assumed to be a positional argument.
    private func parseArgs() throws {

      // For each argument we are passed...
      var passThroughArgs = false
      for arg in CommandLine.arguments[1..<CommandLine.arguments.count] {
        // If the argument doesn't match the optional argument pattern. Add
        // it to the positional argument list and continue...
        if passThroughArgs || !arg.starts(with: "-") {
          positionalArgs.append(arg)
          continue
        }
        if arg == "--" {
          passThroughArgs = true
          continue
        }
        // Attempt to split it into two components separated by an equals sign.
        let components = arg.split(separator: "=")
        let optionName = String(components[0])
        guard validOptions.contains(optionName) else {
          throw ArgumentError.unsupportedArgument(arg)
        }
        var optionVal : String
        switch components.count {
          case 1: optionVal = ""
          case 2: optionVal = String(components[1])
          default:
          // If we do not have two components at this point, we can not have
          // an option switch. This is an invalid argument. Bail!
          throw ArgumentError.unsupportedArgument(arg)
        }
        optionalArgsMap[optionName] = optionVal
      }
    }

    public func addArgument<T>(
      _ name: String?,
      _ property: WritableKeyPath<U, T>,
      defaultValue: T? = nil,
      parser: @escaping (String) throws -> T? = { _ in nil }
    ) {
      arguments.append(Argument(name: name)
        { try self.parseArgument(name, property, defaultValue, parser) })
    }

    private func parseArgument<T>(
      _ name: String?,
      _ property: WritableKeyPath<U, T>,
      _ defaultValue: T?,
      _ parse: (String) throws -> T?
    ) throws {
      if let name = name, let value = optionalArgsMap[name] {
        guard !value.isEmpty || defaultValue != nil
          else { throw ArgumentError.missingValue(name) }

        result[keyPath: property] = (value.isEmpty)
          ? defaultValue!
          : try checked(parse, value, argument: name)
      } else if name == nil {
        result[keyPath: property] = positionalArgs as! T
      }
    }
}
