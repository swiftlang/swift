// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI
import Foundation

protocol P {
  var info: Info? { get }
}

protocol Q : P {
  func makeView() -> AnyView
}

struct Info {
  var kind: Kind

  var size: CGSize { fatalError() }
}

enum Kind {
  case supported([Int])
  case unsupported
}

struct Settings {
  var setting = Setting.defaultValue
}

struct Setting {
  static let defaultValue = Setting()

  func scale(_: CGFloat) -> Setting {
     fatalError()
  }

  func scale(_: CGSize) -> CGSize {
     fatalError()
  }
}

struct Test: View {
  var res: P
  var enable: Bool
  var settings: Settings

  var body: some View {
    if let result = res as? Q {
      let view = result.makeView()

      if let info = result.info {
        let show: Bool = {
          guard enable else { return false }

          switch info.kind {
          case .supported: return true
          case .unsupported: return false
          }
        }()

        if show {
          let size = settings.setting.scale(info.size)
          view.frame(width: size.width, height: size.height)
        }
      }
    }
  }
}
