require "json"
require "optparse"

BASE_MATRIX_ENTRIES = [
  {
    "build_os": "ubuntu-18.04",
    "agent_query": "ubuntu-18.04",
    "target": "ubuntu18.04_x86_64",
    "container": "ghcr.io/swiftwasm/swift-ci:main-ubuntu-18.04",
    "run_stdlib_test": true,
    "run_full_test": false,
    "run_e2e_test": true,
    "build_hello_wasm": true,
    "clean_build_dir": false,
    "free_disk_space": true
  },
  {
    "build_os": "ubuntu-20.04",
    "agent_query": "ubuntu-20.04",
    "target": "ubuntu20.04_x86_64",
    "container": "ghcr.io/swiftwasm/swift-ci:main-ubuntu-20.04",
    "run_stdlib_test": true,
    "run_full_test": false,
    "run_e2e_test": true,
    "build_hello_wasm": true,
    "clean_build_dir": false,
    "free_disk_space": true
  },
  {
    "build_os": "ubuntu-22.04",
    "agent_query": "ubuntu-22.04",
    "target": "ubuntu22.04_x86_64",
    "container": "ghcr.io/swiftwasm/swift-ci:main-ubuntu-22.04",
    "run_stdlib_test": true,
    "run_full_test": false,
    "run_e2e_test": true,
    "build_hello_wasm": true,
    "clean_build_dir": false,
    "free_disk_space": true
  },
  {
    "build_os": "macos-11",
    "agent_query": "macos-11",
    "target": "macos_x86_64",
    "run_stdlib_test": false,
    "run_full_test": false,
    "run_e2e_test": false,
    "build_hello_wasm": false,
    "clean_build_dir": false
  },
  {
    "build_os": "macos-11",
    "agent_query": ["self-hosted", "macOS", "ARM64"],
    "target": "macos_arm64",
    "run_stdlib_test": false,
    "run_full_test": false,
    "run_e2e_test": false,
    "build_hello_wasm": true,
    "clean_build_dir": true
  }
]

def main
  options = {}
  OptionParser.new do |opts|
    opts.banner = "Usage: build-matrix.rb [options]"
    opts.on("--runner [JSON FILE]", "Path to runner data file") do |v|
      options[:runner] = v
    end
  end.parse!

  matrix_entries = BASE_MATRIX_ENTRIES.dup
  if options[:runner]
    runner = JSON.parse(File.read(options[:runner]))
    if label = runner["outputs"]["ubuntu20_04_aarch64-label"]
      matrix_entries << {
        "build_os": "amazon-linux-2",
        "agent_query": label,
        "target": "amazonlinux2_x86_64",
        "run_stdlib_test": false,
        "run_full_test": false,
        "run_e2e_test": false,
        "build_hello_wasm": true,
        "clean_build_dir": false
      }
    end
    if label = runner["outputs"]["ubuntu20_04_aarch64-label"]
      matrix_entries << {
        "build_os": "ubuntu-20.04",
        "agent_query": label,
        "target": "ubuntu20.04_aarch64",
        "run_stdlib_test": false,
        "run_full_test": false,
        "run_e2e_test": false,
        "build_hello_wasm": true,
        "clean_build_dir": false
      }
    end
  end

  print JSON.generate(matrix_entries)
end

if $0 == __FILE__
  main
end
