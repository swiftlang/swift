// RUN: %target-swift-frontend %s -emit-ir

// Test case submitted to project by https://github.com/jansabbe (Jan Sabbe)

class A<B : CollectionType where B : AnyObject> {
}
