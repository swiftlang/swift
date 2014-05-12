// RUN: %target-run-simple-swift | FileCheck %s

func pipe<T>(input: SequenceOf<T>, output: SinkOf<T>) {
  for x in input {
    output.put(x)
  }
}

struct Print<T: FormattedPrintable> : Sink {
  func put(x: T) {
    print(x.format("v", layout: " "))
  }
}

var z = [ 1, 2, 3 ]

func printArray<T: FormattedPrintable>(x: T[]) {
  pipe(SequenceOf(x), SinkOf(Print<T>()))
  println()
}

// CHECK: 1234
// CHECK-NEXT: foo bar bass
printArray([1, 2, 3, 4])
printArray(["foo ", "bar ", "bass"])
