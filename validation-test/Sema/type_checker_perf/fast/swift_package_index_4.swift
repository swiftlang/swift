// RUN: %target-swift-frontend -typecheck %s -solver-scope-threshold=60000
// REQUIRES: tools-release,no_asan

protocol ArgumentProtocol {}

extension String: ArgumentProtocol {}
extension Bool: ArgumentProtocol {}

struct ConcreteError: Error {}

struct Option<T> {
  init(key: String, defaultValue: T, usage: String) {}
}

struct Argument<T> {
  init(defaultValue: T? = nil, usage: String, usageParameter: String? = nil) {}
}

struct Switch {
  init(flag: Character? = nil, key: String, usage: String) {}
}

infix operator <*> : LogicalDisjunctionPrecedence
infix operator <| : MultiplicationPrecedence

func <*> <T, U>(f: (T) -> U, value: Result<T, ConcreteError>) -> Result<U, ConcreteError> { fatalError() }
func <*> <T, U>(f: Result<((T) -> U), ConcreteError>, value: Result<T, ConcreteError>) -> Result<U, ConcreteError> { fatalError() }

struct CommandMode {
  static func <| <T: ArgumentProtocol>(mode: CommandMode, option: Option<T>) -> Result<T, ConcreteError> { fatalError() }
  static func <| <T: ArgumentProtocol>(mode: CommandMode, option: Option<T?>) -> Result<T?, ConcreteError> { fatalError() }
  static func <| <T: ArgumentProtocol>(mode: CommandMode, option: Option<[T]>) -> Result<[T], ConcreteError> { fatalError() }
  static func <| <T: ArgumentProtocol>(mode: CommandMode, option: Option<[T]?>) -> Result<[T]?, ConcreteError> { fatalError() }
  static func <| (mode: CommandMode, option: Option<Bool>) -> Result<Bool, ConcreteError> { fatalError() }
  static func <| (mode: CommandMode, option: Switch) -> Result<Bool, ConcreteError> { fatalError() }
  static func <| <T: ArgumentProtocol>(mode: CommandMode, argument: Argument<T>) -> Result<T, ConcreteError> { fatalError() }
  static func <| <T: ArgumentProtocol>(mode: CommandMode, argument: Argument<[T]>) -> Result<[T], ConcreteError> { fatalError() }
}

struct FileManager {
  static var `default`: FileManager = FileManager()
  var currentDirectoryPath: String = ""
}

struct Options {
  static func create(_ project: String?) -> (String?) -> (String) -> (String) -> (String?) -> (String?) -> (String?) -> (String?) -> (Bool) -> ([String]) -> Options { fatalError() }

  static func evaluate(_ mode: CommandMode) -> Result<Options, ConcreteError> {
    let defaultBuildDirectory = ""
    return create
      <*> mode <| Option(key: "", defaultValue: nil, usage: "")
      <*> mode <| Option(key: "", defaultValue: nil, usage: "")
      <*> mode <| Option(key: "", defaultValue: FileManager.default.currentDirectoryPath, usage: "")
      <*> mode <| Option(key: "", defaultValue: FileManager.default.currentDirectoryPath, usage: "")
      <*> mode <| Option(key: "", defaultValue: nil, usage: "")
      <*> mode <| Option(key: "", defaultValue: nil, usage: "")
      <*> mode <| Option(key: "", defaultValue: nil, usage: "")
      <*> mode <| Option(key: "", defaultValue: nil, usage: "")
      <*> mode <| Switch(key: "", usage: "")
      <*> mode <| Argument(defaultValue: [], usage: "")
  }
}
