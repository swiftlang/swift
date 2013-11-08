// RUN: %swift -i %s | FileCheck %s
// REQUIRES: swift_interpreter

def pipe<T>(input: EnumerableOf<T>, output: SinkOf<T>) {
  for x in input {
    output.put(x)
  }
}

struct Print<T: FormattedPrintable> : Sink {
  def put(x: T) {
    print(x.format('v', " "))
  }
}

var z = [ 1, 2, 3 ]

def printArray<T: FormattedPrintable>(x: T[]) {
  pipe(existential(x), existential(Print<T>()))
  println()
}

// CHECK: 1234
// CHECK-NEXT: foo bar bass
printArray([1, 2, 3, 4])
printArray(["foo ", "bar ", "bass"])
