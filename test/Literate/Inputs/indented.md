Indented Code Block
===================

This should build a program that prints "OK":

    // CHECK: SIMPLE
    print("SIMPLE")

We also want to make sure that adding more blocks works:

    // CHECK: SECOND
    print("SECOND")

And that using more than four space works:

     // CHECK: FIVE SPACES
     print("FIVE SPACES")

Code blocks need a blank line before
    // CHECK-NOT: NEED BLANK
    print("NEED BLANK")

Indented code blocks can contain indentation

    // CHECK: INDENTATION OK. SUM IS 55
    var sum = 0
    for n in 1...10 {
      sum += n
    }
    print("INDENTATION OK. SUM IS \(sum)")

Indented code blocks can also contain blank lines

    // CHECK: BLANK NO SPACES

    print("BLANK NO SPACES")

with or without indentation

    // CHECK: BLANK SPACES
    
    print("BLANK SPACES")
