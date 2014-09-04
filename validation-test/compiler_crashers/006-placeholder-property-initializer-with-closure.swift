// RUN: not --crash %swift -emit-ir %s
// Test case submitted to project by https://github.com/AlexDenisov (Alexey Denisov)

func i(c: () -> ()) {
}

class a {
    var _ = i() {
    }
}
