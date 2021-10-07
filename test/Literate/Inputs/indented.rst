Indented Code Blocks
====================

In reStructuredText, code blocks are introduced with a double colon::

  // CHECK: SIMPLE
  print("SIMPLE")

They run until the indentation level returns to the level of the paragraph that started them.

  So if we start a paragraph here::

    // CHECK: INDENTED
    // CHECK-NOT: AFTER-INDENTED
    print("INDENTED")

  print("AFTER-INDENTED")

is part of the text.

We also support them in bulleted lists:

* One

* Two::

    // CHECK: BULLET2
    print("BULLET2")

* Three::

    // CHECK: BULLET3
    print("BULLET3")

* Four

* Five::

    // CHECK: BULLET5
    print("BULLET5")

and in numbered lists:

1. One

2. Two::

     // CHECK: NUMBERED2
     print("NUMBERED2")

3. Three::

     // CHECK: NUMBERED3
     print("NUMBERED3")

4. Four

5. Five::

     // CHECK: NUMBERED5
     print("NUMBERED5")

17. Seventeen::

      // CHECK: NUMBERED17
      print("NUMBERED17")

#. Automatic::

     // CHECK: NUMBEREDAUTO
     print("NUMBEREDAUTO")

#. Auto2::

     // CHECK: NUMBEREDAUTO2
     print("NUMBEREDAUTO2")

Indented code blocks can contain indentation::

  // CHECK: INDENTATION OK. SUM IS 55
  var sum = 0
  for n in 1...10 {
    sum += n
  }
  print("INDENTATION OK. SUM IS \(sum)")

They can also contain blank lines::

  // CHECK: BLANK NO SPACES

  print("BLANK NO SPACES")

with or without indentation::

  // CHECK: BLANK SPACES
  
  print("BLANK SPACES")
