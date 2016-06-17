// XFAIL: broken_std_regex
// RUN: %complete-test -tok=TOK1 -hide-none %s -- %s

import QuartzCore

class Cl{
  var L : CALayer = {
    let layer = CALayer()
    layer.transform = #^TOK1^#
