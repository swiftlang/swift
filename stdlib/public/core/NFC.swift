//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

extension Unicode {
  internal struct NFC<S: StringProtocol> {
    let base: S
  }
}

extension Unicode.NFC {
  internal struct Iterator {
    var buffer: [(scalar: Unicode.Scalar, normData: UInt16)] = []
    
    var composee: Unicode.Scalar? = nil
    
    var hasBeenReversed = false
    
    var iterator: Unicode.NFD<S>.Iterator
  }
}

extension Unicode.NFC.Iterator: IteratorProtocol {
  internal func compose(
    _ x: Unicode.Scalar,
    and y: Unicode.Scalar
  ) -> Unicode.Scalar? {
    // Fast path: ASCII and some latiny scalars never compose when they're on
    // the rhs.
    if y.value < 0x300 {
      return nil
    }
    
    switch (x.value, y.value) {
    // Check for Hangul (L, V) -> LV compositions.
    case (0x1100 ... 0x1112, 0x1161 ... 0x1175):
      let lIdx = x.value &- 0x1100
      let vIdx = y.value &- 0x1161
      let lvIdx = lIdx &* 588 &+ vIdx &* 28
      let s = 0xAC00 &+ lvIdx
      return Unicode.Scalar(_value: s)
      
    // Check for Hangul (LV, T) -> LVT compositions.
    case (0xAC00 ... 0xD7A3, 0x11A7 &+ 1 ... 0x11C2):
      if (x.value &- 0xAC00) % 28 == 0 {
        return Unicode.Scalar(_value: x.value &+ y.value &- 0x11A7)
      } else {
        fallthrough
      }
      
    // Otherwise, look it up.
    default:
      let composition = _swift_stdlib_getComposition(x.value, y.value)
      
      guard composition != .max else {
        return nil
      }
      
      return Unicode.Scalar(_value: composition)
    }
  }

  internal mutating func next() -> Unicode.Scalar? {
    // Empty out our buffer before attempting to compose anything with our new
    // composee.
    if !buffer.isEmpty {
      if !hasBeenReversed {
        buffer.reverse()
        hasBeenReversed = true
      }
      
      return buffer.removeLast().scalar
    }
    
    hasBeenReversed = false
    
    while let current = iterator.next() {
      let currentCCC = current.normData >> 3
      let currentIsNFCQC = current.normData & 0x6 == 0
      
      guard let l = composee else {
        // If we don't have a composee at this point, we're most likely looking
        // at the start of a string. If our class is 0, then attempt to compose
        // the following scalars with this one. Otherwise, it's a one off scalar
        // that needs to be emitted.
        if currentCCC == 0 {
          composee = current.scalar
          continue
        } else {
          return current.scalar
        }
      }
      
      // Check if we have any scalars within the buffer, and if so get the last
      // scalar's normalization data.
      guard let lastNormData = buffer.last?.normData else {
        // If we do not any have scalars in our buffer yet, then this step is
        // trivial. Attempt to compose our current scalar with whatever composee
        // we're currently building up.
        
        // If our right hand side scalar IS NFC_QC, then that means it can
        // never compose with any scalars previous to it. So, if our current
        // scalar is NFC_QC, then we have no composition.
        guard !currentIsNFCQC, let p = compose(l, and: current.scalar) else {
          // We did not find a composition between the two. If our current class
          // is 0, then set that as the new composee and return whatever built
          // up scalar we have. Otherwise, add our current scalar to the buffer
          // for eventually removal!
          
          guard currentCCC == 0 else {
            buffer.append(current)
            continue
          }
          
          composee = current.scalar
          return l
        }
        
        // We found a composition! Record it as our new composee and repeat the
        // process.
        composee = p
        continue
      }
      
      // We only care about the last's ccc.
      let lastCCC = lastNormData >> 3
      
      // Check if our current scalar is not blocked from our current composee.
      // In this case blocked means there is some scalar whose class (lastClass)
      // is either == 0 or >= currentClass.
      //
      // Example:
      //
      //     "z\u{0335}\u{0327}\u{0324}\u{0301}"
      //
      // In this example, there are several combining marks following a 'z', but
      // none of them actually compose with the composee 'z'. However, the last
      // scalar U+0301 does actually compose. So this check makes sure that the
      // last scalar doesn't have any scalar in between it and the composee that
      // would otherwise "block" it from composing.
      guard lastCCC < currentCCC else {
        // We had a scalar block it. That means our current scalar is either a
        // starter or has a same class (preserve ordering).
        
        guard currentCCC == 0 else {
          // Not a starter, stick it at the end of the buffer and keep going!
          
          buffer.append(current)
          continue
        }
        
        // Starters are the "start" of a new normalization segment. Set it as
        // the new composee and return our current composee. This will trigger
        // any other scalars in the buffer to be emitted before we handle
        // composing this new composee.
        composee = current.scalar
        return l
      }
      
      // There were no blockers! Attempt to compose the two! (Again, if our rhs
      // scalar IS NFC_QC, then it can never compose with anything previous to
      // it).
      guard !currentIsNFCQC, let p = compose(l, and: current.scalar) else {
        // No composition found. Stick it at the end of the buffer with the rest
        // of non-composed scalars.
        
        buffer.append(current)
        continue
      }
      
      // They composed! Assign the composition as our new composee and iterate
      // to the next scalar.
      composee = p
    }
    
    // If we have a leftover composee, make sure to return it.
    return composee.take()
  }
}

extension Unicode.NFC: Sequence {
  internal func makeIterator() -> Iterator {
    Iterator(iterator: base.nfd.makeIterator())
  }
}

extension StringProtocol {
  internal var nfc: Unicode.NFC<Self> {
    Unicode.NFC(base: self)
  }
}
