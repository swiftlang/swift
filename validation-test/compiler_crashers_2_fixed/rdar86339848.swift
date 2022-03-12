// RUN: %target-swift-frontend %s -emit-silgen

struct Boom: Decodable {}

enum Whiz: Decodable {
  case bang(_ boom: Boom)
}
