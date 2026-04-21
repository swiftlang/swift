// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100
// REQUIRES: OS=macosx

import Foundation

// https://github.com/swiftlang/swift/issues/45155

let defaults = UserDefaults.standard
defaults.register(defaults: [
  "ShadowsocksOn": true,
  "ShadowsocksRunningMode": "auto",
  "LocalSocks5.ListenPort": NSNumber(value: 1086),
  "LocalSocks5.ListenAddress": "127.0.0.1",
  "PacServer.ListenAddress": "127.0.0.1",
  "PacServer.ListenPort":NSNumber(value: 8090),
  "LocalSocks5.Timeout": NSNumber(value: 60),
  "LocalSocks5.EnableUDPRelay": NSNumber(value: false),
  "LocalSocks5.EnableVerboseMode": NSNumber(value: false),
  "GFWListURL": "https://raw.githubusercontent.com/gfwlist/gfwlist/master/gfwlist.txt",
  "WhiteListURL": "https://raw.githubusercontent.com/breakwa11/gfw_whitelist/master/whitelist.pac",
  "WhiteListIPURL": "https://raw.githubusercontent.com/breakwa11/gfw_whitelist/master/whiteiplist.pac",
  "AutoConfigureNetworkServices": NSNumber(value: true)
])
