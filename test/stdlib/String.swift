// RUN: %swift -i  %s

func [asmname="swift_startBenchmark"] startBenchmark() 
     -> UInt64
func [asmname="swift_printBenchmark"] printBenchmark(
     start : UInt64, laps : UInt64, s : String)

func testAppendSpeed() {
  var start = startBenchmark()
  var x = "The quick brown fox jumped over the lazy dog".split(' ')

  var story = "Let me tell you a story:"
  var laps = 10000

  for i in 0..laps {
    for s in x {
       story += ' '
       story += s
    }
    story += '.'
  }

  printBenchmark(start, UInt64(laps), "for a sequence of String appends")
  println("Was that intolerably slow?  If so, we've regressed")
}
testAppendSpeed()

