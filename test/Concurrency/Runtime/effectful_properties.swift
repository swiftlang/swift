// RUN: %target-run-simple-swift(-parse-as-library %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

enum GeneralError : Error {
  case UnknownBallKind
  case Todo
}

enum BallKind {
  case MostLostV1
  case Chromehard
  case PT5
  case KirksandLignature
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
class Specs {
  // obtains the number of dimples
  subscript(_ bk : BallKind) -> Int {
    get throws {
      switch (bk) {
      case .MostLostV1:
        return 450
      case .Chromehard:
        return 125
      default:
        throw GeneralError.UnknownBallKind
      }
    }
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
actor Database {
  var currentData : Specs {
    get async {
      let handle = detach { Specs() }
      print("obtaining specs...")
      return await handle.get()
    }
  }

  var hasNewData : Bool {
    get throws { return true }
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
protocol SphericalObject {
  var name : String { get async throws }
  var dimples : Int { get async throws }
  var description : String { get async throws }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
class Ball : SphericalObject {
  var name : String { get async throws { throw GeneralError.Todo } }
  var dimples : Int { get async throws { throw GeneralError.Todo } }
  var description : String { get async throws { throw GeneralError.Todo } }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
class GolfBall : Ball {
  private static let db : Database = Database()

  private var _model : BallKind
  private var _dimples : Int?

  init(_ bk : BallKind) {
    _model = bk
  }

  override var name : String {
    return "golf ball"
  }

  override var description : String {
    get async throws {
      return "this \(name) has \(await dimples) dimples"
    }
  }

  override var dimples : Int {
     get async {
      let newData = (try? await GolfBall.db.hasNewData) ?? false

      if newData || _dimples == nil {
        let specs = await GolfBall.db.currentData
        _dimples = (try? specs[_model]) ?? 0
      }

      return _dimples!
    }
  }
}

// CHECK: obtaining specs...
// CHECK: this golf ball has 450 dimples
// CHECK: obtaining specs...
// CHECK: this golf ball has 125 dimples
// CHECK: obtaining specs...
// CHECK: this golf ball has 0 dimples
// CHECK: obtaining specs...
// CHECK: this golf ball has 0 dimples

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func printAsBall(_ b : Ball) async {
  print(try! await b.description)
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func printAsAsSphericalObject(_ b : SphericalObject) async {
  print(try! await b.description)
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct RunIt {
  static func main() async {
    let balls : [(Bool, Ball)] = [
      (true, GolfBall(.MostLostV1)),
      (false, GolfBall(.Chromehard)),
      (true, GolfBall(.PT5)),
      (false, GolfBall(.KirksandLignature))
    ]
    for (useProtocol, ball) in balls {
      if (useProtocol) {
        await printAsAsSphericalObject(ball)
      } else {
        await printAsBall(ball)
      }
    }
  }
}
