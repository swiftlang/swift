// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5 -disable-availability-checking
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

struct FullScreenView<Content>: View where Content: View {
  @State private var showFullScreen: Bool = false

  var x: Double = 0
  var y: Double = 0

  var content: () -> Content

  // Body of this view used to cause rapid memory use.
  var body: some View {
    ZStack {
      VStack {
        HStack {
          Button {
          } label: {
            Text("a")
          }
          .offset(x: x, y: y)
        }
      }
    }
    .fullScreenCover(isPresented: $showFullScreen) {    
      ZStack {
        VStack {
          HStack {
            Button {
	      
            } label: {
              Text("a")
            }
          }
        }
      }
    }
  }
}
