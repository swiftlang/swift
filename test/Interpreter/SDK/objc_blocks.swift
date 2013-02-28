// RUN: %swift -sdk=%sdk -constraint-checker -i %s | FileCheck %s
// REQUIRES: sdk

import Foundation

func getBlockWithContext(suffix:String)
  -> (NSString, CPointer<BOOL>) -> ()
{
  return func(line, stop) {
    print(line)
    println(suffix)
  }
}

// CHECK: apple cha! cha! cha!
// CHECK: banana cha! cha! cha!
// CHECK: cherry cha! cha! cha!
// CHECK: grape cha! cha! cha!
NSString("apple\nbanana\ncherry\ngrape\n").enumerateLinesUsingBlock(
  func(line, _) {
    print(line)
    println(" cha! cha! cha!")
  })

// CHECK: apple talkin' bout my generation
// CHECK: banana talkin' bout my generation
// CHECK: cherry talkin' bout my generation
// CHECK: grape talkin' bout my generation
NSString("apple\nbanana\ncherry\ngrape\n").enumerateLinesUsingBlock(
  getBlockWithContext(" talkin' bout my generation"))

