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
    if passThroughArgs || !arg.characters.starts(with: "-".characters) {
      positionalArgs.append(arg)
      continue
    }
    if arg == "--" {
      passThroughArgs = true
      continue
    }
    // Attempt to split it into two components separated by an equals sign.
    let components = arg.components(separatedBy: "=")
    let optionName = components[0]
    if validOptions != nil && !validOptions!.contains(optionName) {
      print("Invalid option: \(arg)")
      return nil
    }
    var optionVal : String
    switch components.count {
      case 1: optionVal = ""
      case 2: optionVal = components[1]
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
