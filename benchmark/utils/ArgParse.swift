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

public struct Arguments {
  public var progName: String
  public var positionalArgs: [String]
  public var optionalArgsMap: [String : String]

  init(_ pName: String, _ posArgs: [String], _ optArgsMap: [String : String]) {
    progName = pName
    positionalArgs = posArgs
    optionalArgsMap = optArgsMap
  }
}

enum ArgumentError: Error {
  case missingValue(String)
  case invalidType(value: String, type: String, argument: String?)
  case general(String)
}

extension ArgumentError: CustomStringConvertible {
  public var description: String {
    switch self {
    case let .missingValue(key):
      return "\(key) requires a value" //"Missing value for `\(key)`"
    case let .invalidType(value, type, argument):
      return (argument == nil)
        ? "'\(value)' is not a valid '\(type)'"
        : "'\(value)' is not a valid '\(type)' for '\(argument!)'"
    case let .general(description):
      return "\(description)"
    }
  }
}

/// Using CommandLine.arguments, returns an Arguments struct describing
/// the arguments to this program. If we fail to parse arguments, we
/// return nil.
///
/// We assume that optional switch args are of the form:
///
/// --opt-name[=opt-value]
/// -opt-name[=opt-value]
///
/// with opt-name and opt-value not containing any '=' signs. Any
/// other option passed in is assumed to be a positional argument.
public func parseArgs(_ validOptions: [String]? = nil)
  -> Arguments? {
  let progName = CommandLine.arguments[0]
  var positionalArgs = [String]()
  var optionalArgsMap = [String : String]()

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
    if validOptions != nil && !validOptions!.contains(optionName) {
      print("Invalid option: \(arg)")
      return nil
    }
    var optionVal : String
    switch components.count {
      case 1: optionVal = ""
      case 2: optionVal = String(components[1])
      default:
      // If we do not have two components at this point, we can not have
      // an option switch. This is an invalid argument. Bail!
      print("Invalid option: \(arg)")
      return nil
    }
    optionalArgsMap[optionName] = optionVal
  }

  return Arguments(progName, positionalArgs, optionalArgsMap)
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

struct Argument {
  let name: String?
  let apply: () throws -> ()
}

class ArgumentParser<U> {
    var result: U
    var validOptions: [String] {
      return arguments.compactMap { $0.name }
    }
    private var benchArgs: Arguments!
    private var arguments: [Argument] = []

    init(into result: U) {
      self.result = result
      self.arguments += [
        Argument(name: "--help", apply: printUsage)
      ]
    }

    func printUsage() {
      guard let _ = benchArgs.optionalArgsMap["--help"] else { return }
      print("Valid options:")
      for v in validOptions {
        print("    \(v)")
      }
      exit(0)
    }

    func parse() -> U {
      do {
        guard let benchArgs = parseArgs(validOptions) else {
          throw ArgumentError.general("Failed to parse arguments")
        }
        self.benchArgs = benchArgs
        try arguments.forEach { try $0.apply() } // parse all arguments
        return result
      } catch let error as ArgumentError {
        fflush(stdout)
        fputs("\(error)\n", stderr)
        fflush(stderr)
        exit(1)
      } catch {
        fatalError("\(error)")
      }
    }

    func addArgument<T>(
      _ name: String?,
      _ property: WritableKeyPath<U, T>,
      defaultValue: T? = nil,
      parser: @escaping (String) throws -> T? = { _ in nil }
    ) {
      arguments.append(Argument(name: name)
        { try self.parseArgument(name, property, defaultValue, parser) })
    }

    func parseArgument<T>(
      _ name: String?,
      _ property: WritableKeyPath<U, T>,
      _ defaultValue: T?,
      _ parse: (String) throws -> T?
    ) throws {
      if let name = name, let value = benchArgs.optionalArgsMap[name] {
        guard !value.isEmpty || defaultValue != nil
          else { throw ArgumentError.missingValue(name) }

        result[keyPath: property] = (value.isEmpty)
          ? defaultValue!
          : try checked(parse, value, argument: name)
      } else if name == nil {
        result[keyPath: property] = benchArgs.positionalArgs as! T
      }
    }
}
