//===--------------- BuildModulesFromGraph.swift --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
import Foundation

let fileName = CommandLine.arguments[1]
let swiftPath = CommandLine.arguments[2]
let moduleName = CommandLine.arguments[3]
let data = try! Data(contentsOf: URL(fileURLWithPath: fileName))

let decoder = JSONDecoder()
let moduleDependencyGraph = try! decoder.decode(
  ModuleDependencyGraph.self, from: data)

func findModuleBuildingCommand(_ moduleName: String) -> [String]? {
  for (_, dep) in moduleDependencyGraph.modules {
    if URL(fileURLWithPath: dep.modulePath).lastPathComponent == moduleName {
      switch dep.details {
      case .swift(let details):
        return details.commandLine
      case .clang(let details):
        return details.commandLine
      }
    } else {
      continue
    }
  }
  return nil
}

if let command = findModuleBuildingCommand(moduleName) {
  var result = swiftPath
  command.forEach { result += " \($0)"}
  // Pass down additional args to the Swift invocation.
  CommandLine.arguments.dropFirst(4).forEach { result += " \($0)"}
  print(result)
  exit(0)
} else {
  fatalError("cannot find module building commands for \(moduleName)")
}
