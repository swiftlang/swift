// RUN: not %target-swift-frontend %s -parse

// Issue found by https://github.com/jtbandes (Jacob Bandes-Storch)
// <rdar://23720006>

func~=(()->(}switch 0{case 0
