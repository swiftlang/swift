@_private(sourceFile: "dynamic_replacement_dlclose.swift") import Module1

extension A {
  @_dynamicReplacement(for: value)
  var repl: Int {
    return 2
  }
}
