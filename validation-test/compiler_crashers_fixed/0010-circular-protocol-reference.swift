// RUN: %target-swift-frontend %s -emit-ir

// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/18061131

protocol a {
    func c(b: b)
}
 
protocol b {
    func d(a: a)
}
