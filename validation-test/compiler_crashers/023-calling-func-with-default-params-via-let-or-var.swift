// RUN: not --crash %swift %s -emit-ir
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/18041799
// https://gist.github.com/stigi/336a9851cd80fdef22ed

func a(b: Int = 0) {
}

let c = a
c()
