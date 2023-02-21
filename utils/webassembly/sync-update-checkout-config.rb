#!/usr/bin/env ruby

require "json"

DOWNSTREAM_FORKS = ["swift", "swift-corelibs-foundation", "swift-corelibs-xctest"]

def sync_checkout(options)
  config = JSON.parse(File.read(options[:config_path]))
  branch_schemes = config["branch-schemes"]
  upstream = branch_schemes[options[:upstream]]
  raise "No branch scheme for #{options[:upstream]}" unless upstream
  downstream = branch_schemes[options[:downstream]]
  raise "No branch scheme for #{options[:downstream]}" unless downstream

  upstream["repos"].each do |project, upstream_branch|
    next if DOWNSTREAM_FORKS.include?(project)
    downstream_branch = downstream["repos"][project]
    next if downstream_branch == upstream_branch

    puts "Please update #{project} to #{upstream_branch} (currently #{downstream_branch})"
  end
end

def main
  require "optparse"
  options = {
    config_path: File.expand_path(File.join(__dir__, "..", "update_checkout", "update-checkout-config.json")),
    upstream: "main",
    downstream: "wasm",
  }

  opts = OptionParser.new do |opts|
    opts.banner = "Usage: sync-update-checkout.rb [options]"

    opts.on("--config PATH", "Path to update-checkout-config.json") do |path|
      options[:config_path] = path
    end

    opts.on("--upstream BRANCH", "Name of upstream branch") do |branch|
      options[:upstream] = branch
    end

    opts.on("--downstream BRANCH", "Name of downstream branch") do |branch|
      options[:downstream] = branch
    end
  end
  opts.parse!

  sync_checkout(options)
end

main if $0 == __FILE__
