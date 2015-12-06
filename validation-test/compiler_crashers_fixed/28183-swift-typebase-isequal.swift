// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case found by https://github.com/jtbandes (Jacob Bandes-Storch)
// <rdar://23720006>

func~=(()->(}switch 0{case 0
