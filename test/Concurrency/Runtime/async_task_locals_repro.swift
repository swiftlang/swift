// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import _Concurrency

struct MyError: Error {}
struct Image {
}
@available(SwiftStdlib 5.5, *)
func downloadImage(from url: String) async throws -> Image {
  await Task.sleep(10_000)
  return Image()
}
@available(SwiftStdlib 5.5, *)
actor ImageDownloader {
  private enum CacheEntry {
    case inProgress(Task<Image, Error>)
    case ready(Image)
  }
  private var cache: [String: CacheEntry] = [:]
  func image(from url: String) async throws -> Image? {
    if let cached = cache[url] {
      switch cached {
      case .ready(let image):
        return image
      case .inProgress(let handle):
        return try await handle.value
      }
    }
    let handle = Task {
      try await downloadImage(from: url)
    }
    cache[url] = .inProgress(handle)
    do {
      let image = try await handle.value
      cache[url] = .ready(image)
      return image
    } catch {
      cache[url] = nil
      throw error
    }
  }
}
@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    let downloader = ImageDownloader()
    async let image1 = downloader.image(from: "https://homepages.cae.wisc.edu/~ece533/images/airplane.png")
    async let image2 = downloader.image(from: "https://homepages.cae.wisc.edu/~ece533/images/airplane.png")
    async let image3 = downloader.image(from: "https://homepages.cae.wisc.edu/~ece533/images/airplane.png")
    print("\(try! await image1!)")
    print("\(try! await image2!)")
    print("\(try! await image3!)")
    print("okey") // CHECK: OK
  }
}
