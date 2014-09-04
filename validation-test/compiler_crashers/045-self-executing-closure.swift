// RUN: not --crash %swift %s -emit-ir
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17317691

func f() {
    ({})
}
