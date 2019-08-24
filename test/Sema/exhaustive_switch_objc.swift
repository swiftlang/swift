// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s
// REQUIRES: objc_interop

import Foundation

// Treat irrefutable casts as irrefutable patterns.
enum Castbah {
  case shareef(NSInteger)
  case dont(NSString)
  case like(Int)
  case it(Error)
}

func rock(the c : Castbah) {
  switch (c, c, c) {
  case (.shareef(let rock as NSObject), .dont(let the as String), .like(let castbah as Any)):
    print(rock, the, castbah)
  case (.it(let e as NSError), _, _):
    print(e)
  case let obj as Any:
    print(obj)
  }
}
