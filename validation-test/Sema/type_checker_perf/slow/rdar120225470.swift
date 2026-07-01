// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx12 -swift-version 5 -solver-scope-threshold=100000
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import Photos
import SwiftUI

// Stand-in for SwiftUI's `@State`, which became a macro in recent SDKs and would
// otherwise require the SwiftUIMacros plugin. This test exercises type-checking
// performance, not `@State` itself; the box reproduces `@State`'s nonmutating
// setter and `Binding` projected value.
@propertyWrapper
struct FakeState<Value> {
  final class Box { var value: Value; init(_ value: Value) { self.value = value } }
  private let box: Box
  init(wrappedValue: Value) { box = Box(wrappedValue) }
  var wrappedValue: Value {
    get { box.value }
    nonmutating set { box.value = newValue }
  }
  var projectedValue: Binding<Value> {
    Binding(get: { box.value }, set: { box.value = $0 })
  }
}

struct AssetBrowserView: View {
	@FakeState private var assetsFetchResult: PHFetchResult = { () -> PHFetchResult<PHAsset> in
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
