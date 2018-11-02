// RUN: %target-swift-frontend -emit-ir -o- -parse-as-library -module-name test -validate-tbd-against-ir=missing %s
// RUN: %target-swift-frontend -enable-resilience -emit-ir -o- -parse-as-library -module-name test -validate-tbd-against-ir=missing %s

class C {
}

enum SinglePayload {
  case A
  case B(C)
  case D
}

enum MultiPayload {
  case A
  case B(C)
  case D(C)
}
