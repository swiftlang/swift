// % RUN: %target-swift-frontend -emit-sil -verify %s

func f1() {
  _ = 1..<0 //expected-error{{static error: Can't form Range with end < start}}
}

func f2() {
  _ = 1...0 //expected-error{{static error: Can't form Range with end < start}}
}

func f3() {
  _ = 0...Int.max //expected-error{{static error: Range end index has no valid successor}}
}
