fileprivate enum BundleComponent {
  // RUN: %sourcekitd-test -req=complete -pos=%(line + 1):25 %s -- %s
  case swiftVersions = 's'
}
