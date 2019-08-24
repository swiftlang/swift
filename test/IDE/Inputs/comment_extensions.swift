/// - attention: This function is so hip and exciting, it can't be trusted.
func attention() {}

/// - author: Stephen
func author() {}

/// - authors:
///   - Homer
///   - Mark
///   - J.
func authors() {}

/// - bug: rdar://problem/8675309
func bug() {}

/// - complexity: O(n log2(n))
func complexity() {}

/// - copyright: 2015 Apple, Inc.
func copyright() {}

/// - date: Thu Apr 23 22:38:09 PDT 2015
func date() {}

/// - experiment: Try some more. The strawberries taste like strawberries.
func experiment() {}

/// - invariant: x not nil
struct Invariant {
  let x: Int!
}

/// - note: This function is very hip and exciting.
func note() {}

/// - postcondition: x is unchanged
func postcondition(_ x: inout Int) {}

/// - precondition: `x < 100`
func precondition(_ x: Int) {
  assert(x < 100)
}

/// - remark: Always, no, never forget to check your references.
func remark() {}

/// - remarks:
///   - Never let a bear approach you.
func remarks() {}

/// - requires:
///   - explicit package name. Just kidding!
func requires() {}

/// - seealso: the pie (it's very good).
func see() {}

/// - since: 1809
func since() {}

/// - todo: be
/// - todo: or not to be
func todo() {}

/// - version: Beta.
func version() {}

/// - warning: Share the road.
func warning() {}

/// ![/bogus/url/as/title]()
func imageWithEmptyURLAndBogusTitle () {}

/// Brief.
///
/// ![Image Alt](/swift.png "Image Title")
func imageTitleAndAlt() {}

/// Brief.
///
/// ![Image *Alt*](/swift.png)
func imageAlt() {}

/// Brief.
///
/// ![Image _Alt_](/swift.png "Image Title")
func imageTitle() {}

/// Brief.
///
/// Test [a link](http://apple.com?a=1&b=1&c=abc)
func urlWithQueryString() {}

/// Brief.
///
/// ![&&&](http://apple.com "&&&")
func imageWithAmpersandsInTitleAndAlt() {}
