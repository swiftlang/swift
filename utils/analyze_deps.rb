#!/usr/bin/env ruby
require 'yaml'
require 'set'

Private = Struct.new(:name)

if ARGV.first == '--all'
  ARGV.shift
elsif ARGV.first == '--no-private'
  ARGV.shift
  YAML::add_domain_type('developer.apple.com,2014', 'private') do |type, val|
    nil
  end
else
  YAML::add_domain_type('developer.apple.com,2014', 'private') do |type, val|
    Private.new(val)
  end
end

files = {}
ARGV.each do |file|
  yaml = YAML::load_file(file)
  files[File.basename(file)] = yaml if yaml
end

YAML::dump(files)

# We cheat here and dump everything into the same bucket.
names = {}
files.each do |k,v|
  v['provides'].each do |name|
    names[name] ||= []
    names[name] << k
  end if v['provides']
  v['nominals'].each do |name|
    names[name] ||= []
    names[name] << k
  end if v['nominals']
  v['class-members'].each do |name|
    names[name] ||= []
    names[name] << k
  end if v['class-members']
end

all_deps = {}
all_private_deps = {}
files.each do |k,v|
  deps = Set.new()
  private_deps = Set.new()
  v['top-level'].each do |name|
    next unless name
    if name.is_a? Private
      private_deps.merge(names[name.name] || [])
    else
      deps.merge(names[name] || [])
    end
  end if v['top-level']
  v['member-access'].each do |name|
    next unless name
    if name.is_a? Private
      private_deps.merge(names[name.name] || [])
    else
      deps.merge(names[name] || [])
    end
  end if v['member-access']
  v['dynamic-lookup'].each do |name|
    next unless name
    if name.is_a? Private
      private_deps.merge(names[name.name] || [])
    else
      deps.merge(names[name] || [])
    end
  end if v['dynamic-lookup']
  deps.delete(k)
  private_deps.delete(k)
  private_deps.subtract(deps)
  all_deps[k] = deps.to_a.sort
  all_private_deps[k] = private_deps.to_a.sort
end

# Graphviz output!
puts 'digraph dependencies {'
all_deps.each do |k,v|
  v.each do |dep|
    puts "\t\"#{dep}\" -> \"#{k}\""
  end
  if v.empty?
    puts "\t\"#{k}\""
  end
end
all_private_deps.each do |k,v|
  v.each do |dep|
    puts "\t\"#{dep}\" -> \"#{k}\" [style=dotted]"
  end
end
puts '}'
