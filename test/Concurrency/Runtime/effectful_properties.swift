// RUN: %target-run-simple-swift(-parse-as-library -target %target-swift-5.1-abi-triple) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: freestanding

// rdar://76038845
// REQUIRES: concurrency_runtime
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

@available(SwiftStdlib 5.1, *)
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

@available(SwiftStdlib 5.1, *)
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

@available(SwiftStdlib 5.1, *)
protocol SphericalObject {
  var name : String { get async throws }
  var dimples : Int { get async throws }
  var description : String { get async throws }
}

@available(SwiftStdlib 5.1, *)
class Ball : SphericalObject {
  var name : String { get async throws { throw GeneralError.Todo } }
  var dimples : Int { get async throws { throw GeneralError.Todo } }
  var description : String { get async throws { throw GeneralError.Todo } }
}

@available(SwiftStdlib 5.1, *)
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

@available(SwiftStdlib 5.1, *)
func printAsBall(_ b : Ball) async {
  print(try! await b.description)
}

@available(SwiftStdlib 5.1, *)
func printAsAsSphericalObject(_ b : SphericalObject) async {
  print(try! await b.description)
}

@available(SwiftStdlib 5.1, *)
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
