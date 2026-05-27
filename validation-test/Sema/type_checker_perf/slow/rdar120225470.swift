// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx12 -swift-version 5 -solver-scope-threshold=100000
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import Photos
import SwiftUI

struct AssetBrowserView: View {
	@State private var assetsFetchResult: PHFetchResult = { () -> PHFetchResult<PHAsset> in
    fatalError()
	}()
	
  var body: some View {
    NavigationStack {
      Text("")
      ScrollView{
        ForEach(0..<(Int(assetsFetchResult.count/3))) { i in
          HStack {
            ForEach(0..<3) { j in
              ZStack(alignment: .center) {
                Image(uiImage: PHAsset.getUIImageFromPHAsset(asset: assetsFetchResult[j]()!)!)  // expected-error {{reasonable time}}
                  .frame(width: 0, height: 0)
              }
              .frame(width: 0, height: 0)
            }
          }
        }
      }
    }
    
    .toolbar {
      ToolbarItem(placement: .navigationBarTrailing) {
        SwiftUI.Button() {
        }	label: {
          Image(systemName: "")
        }.foregroundColor(.primary)
      }
      ToolbarItem(placement: .navigationBarTrailing) {
        SwiftUI.Button() {
        } label: {
          Image(systemName: "")
        }.foregroundColor(.primary)
      }
    }
  }
}
