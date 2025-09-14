// RUN: %target-typecheck-verify-swift

protocol PipelineItem {
    associatedtype Input
    associatedtype Output
}

// This example will never work; eventually, it will produce a more precise diagnostic.
func bad<First : PipelineItem,
         each T : PipelineItem,
         Last : PipelineItem>(_ first: First, _ rest: repeat each T, last: Last)
    where (First.Output, repeat (each T).Output) == (repeat (each T).Input, Last.Input) {}
    // expected-error@-1 2{{same-type requirements between packs and concrete types are not yet supported}}

// This is also invalid.
func bad2<First : PipelineItem,
          each T : PipelineItem>(_ first: First, _ rest: repeat each T)
    where repeat (each T).Output == (First, repeat (each T).Input) {}
    // expected-error@-1 {{same-type requirements between packs and concrete types are not yet supported}}

// This could eventually be made to work.
func good<First : PipelineItem,
          each T : PipelineItem>(_ first: First, _ rest: repeat each T)
    where (repeat (each T).Output) == (First, repeat (each T).Input) {}
    // expected-error@-1 {{generic signature requires types '(repeat (each T).Output)' and '(First, repeat (each T).Input)' to be the same}}
