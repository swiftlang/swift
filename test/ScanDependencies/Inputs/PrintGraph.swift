import Foundation

let fileName = CommandLine.arguments[1]
let data = try! Data(contentsOf: URL(fileURLWithPath: fileName))

let decoder = JSONDecoder()
let moduleDependencyGraph = try! decoder.decode(
  ModuleDependencyGraph.self, from: data)
print(moduleDependencyGraph)
