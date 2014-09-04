// RUN: not --crash %swift %s -emit-ir
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// rdar://17242441
class a<f : b, g : b where f.d == g> {
}
protocol b {
    typealias d 
    typealias e
}
struct c<h : b> : b {
    typealias d = h
    typealias e = a<c<h>, d>
}