// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -parse-as-library -primary-file %S/Inputs/objc_multi_file_2.swift %s -verify

// REQUIRES: objc_interop

import Foundation

class UIImage : NSObject { }

@objc
protocol ImagePresentingView {
  var hidden: Bool { @objc(isHidden) get set }
}
