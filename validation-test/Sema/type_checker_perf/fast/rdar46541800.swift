// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=macosx

import simd
import CoreGraphics
import Foundation

class SomeView {
  func layoutSubviews() {
    let descriptionTextViewFrame = CGRect.zero
    let availableBounds = CGRect()
    let descriptionLabelProperties = SomeView.descriptionTextViewLabelProperties()
    let textSize = descriptionTextView.sizeThatFits(availableBounds.size)
    let textInset = descriptionTextView.textInset(forBounds: CGRect(origin: .zero, size: textSize))
    let descriptionTextBaselineOffset: CGFloat = CGFloat()
    let displayScale: CGFloat = CGFloat()
    let _ = (descriptionTextViewFrame.height 
             + (-descriptionTextView.lastBaselineOffsetFromBottom - textInset.bottom + descriptionLabelProperties.lastBaselineOffsetFromBottom)
            + (-descriptionTextView.firstBaselineOffsetFromTop - textInset.top + descriptionTextBaselineOffset).ceilingValue(scale: displayScale)
            )
  }

  static func descriptionTextViewLabelProperties() -> FontDescriptorBaselineProperties {
      fatalError()
  }
  
  lazy var descriptionTextView: SomeOtherView = SomeOtherView()
}

class SomeOtherView {
  init() { }
  func sizeThatFits(_ size: CGSize) -> CGSize { return size }
}


struct FontDescriptorBaselineProperties {
 //   let fontDescriptor: MPUFontDescriptor
    let defaultFirstBaselineOffsetFromTop: CGFloat
    let defaultLastBaselineOffsetFromBottom: CGFloat
    var firstBaselineOffsetFromTop: CGFloat { fatalError() }    
    var lastBaselineOffsetFromBottom: CGFloat {
        fatalError()
    }
}

struct EdgeInsets {
  var top: CGFloat
  var bottom: CGFloat
}

extension SomeOtherView {
    func textInset(forBounds bounds: CGRect) -> EdgeInsets { fatalError() }
    var firstBaselineOffsetFromTop: CGFloat { fatalError() }    
    var lastBaselineOffsetFromBottom: CGFloat {
        fatalError()
    }
}

extension CGFloat {
    public func ceilingValue(scale: CGFloat) -> CGFloat { fatalError() }
}
