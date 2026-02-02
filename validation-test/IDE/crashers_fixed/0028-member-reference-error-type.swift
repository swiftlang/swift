// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE

struct PopularityBadge {
    init(


#if DEBUG
var previews {
    PopularityBadge(score: #^COMPLETE^#
