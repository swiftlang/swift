// RUN: %target-swift-frontend %s -emit-ir

// Issue found by https://github.com/jansabbe (Jan Sabbe)

class A<B : Collection where B : AnyObject> {
}
