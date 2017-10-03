// RUN: %target-swift-frontend %s -emit-ir -o /dev/null

protocol P {
  associatedtype A
}
struct Straint<C: P> where C.A : P {
 typealias X = Any 
}
protocol Q : Straint<Self>.X {}
