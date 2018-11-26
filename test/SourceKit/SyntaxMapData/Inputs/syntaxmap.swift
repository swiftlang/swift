// This is a comment.
// This is a comment that contradicts the previous comment.

class Foo {
    var test : Int
    var a, b : Int
}

#if CONF
var g : Int
#else
var g : Float
#endif

// Something. FIXME: Blah.
// Something http://www.apple.com something else
//http://www.apple.com
// http://www.apple.com

var str = "some string \(0)"

func <#test1#>() {}

/// Brief description.
///
/// Some words.
///
/// - Parameters:
///   - x: ...
/// - z
/// - Notafield: THISISNOTAFIELD
///
/// - parameter y: A number
/// - returns: `x + y`
func foo(x: Int, y: Int) -> Int { return x + y }

/**
  - note: NOTENOTENOTE
- warning: WARNWARNWARN
- returns nothing
  - RETURNS: Void
    - requires: List items to be at the top level
*/
func bar() {}

// mailto:awesomeguy@apple.com
// radar:1234567
// mailto:thisisnotmail
// unknownprotocol://awesomeguy.com

_ = -123

func testArgumentLabels(in class: Int, _ case: (_ default: Int) -> Void) -> (in: Int, String) {
  let result: (in: Int, String) = (0, "test")
  return something ? result : (in: 2, "foo")
}
