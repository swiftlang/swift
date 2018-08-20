// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import Photos

#if os(iOS) || os(tvOS)

if #available(iOS 8.0, tvOS 10.0, *) {
  // compile time only validation test for the SDK overlay,
  // because PHChange pretty much requires a GUI app
  struct GenericPHChangeTest {
    let asset: PHAsset!
    let collection: PHAssetCollection!
    let list: PHCollectionList!
    let assetsFetch: PHFetchResult<PHAsset>!
    let collectionsFetch: PHFetchResult<PHAssetCollection>!
    let listsFetch: PHFetchResult<PHCollectionList>!

    func testPHChange(change: PHChange) {
      let assetDetails: PHObjectChangeDetails<PHAsset>?
        = change.changeDetails(for: asset)
      let collectionDetails: PHObjectChangeDetails<PHAssetCollection>?
        = change.changeDetails(for: collection)
      let listDetails: PHObjectChangeDetails<PHCollectionList>?
        = change.changeDetails(for: list)

      let assetsFetchDetails: PHFetchResultChangeDetails<PHAsset>?
        = change.changeDetails(for: assetsFetch)
      let collectionsFetchDetails: PHFetchResultChangeDetails<PHAssetCollection>?
        = change.changeDetails(for: collectionsFetch)
      let listsFetchDetails: PHFetchResultChangeDetails<PHCollectionList>?
        = change.changeDetails(for: listsFetch)
      assert(assetDetails != nil)
      assert(collectionDetails != nil)
      assert(listDetails != nil)
      assert(assetsFetchDetails != nil)
      assert(collectionsFetchDetails != nil)
      assert(listsFetchDetails != nil)
    }
  }
}

#elseif os(macOS)

if #available(macOS 10.14, *) {
  // compile time only validation test for the SDK overlay,
  // because PHProjectChangeRequest pretty much requires a GUI app
  struct GenericPHProjectChangeRequest {
    let asset: PHAsset!
    let assetsFetch: PHFetchResult<PHAsset>!
    
    func testPHProjectChangeRequest(changeRequest: PHProjectChangeRequest) {
        changeRequest.removeAssets(assetsFetch)
        changeRequest.removeAssets([asset])
    }
  }
  
}

#endif
