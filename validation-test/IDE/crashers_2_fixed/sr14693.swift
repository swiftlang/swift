// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE

@resultBuilder public struct ViewBuilder2 {
    public static func buildBlock<Content>(_ content: Content) -> Never
}

public struct NavigationLink2<Label, Destination> {
    public init(destination: Destination, @ViewBuilder2 label: () -> Label)
    public init(_ titleKey: String, destination: Destination)
}

struct MoviePosterImage2 {
    var imageLoader: Movie2
}
struct MovieDetail2 {}
struct Movie2 {}

struct MoviesHomeGridMoviesRow {
    func foo() {
        let movie: Movie2
        _ = NavigationLink2(destination: MovieDetail2()) {
            MoviePosterImage2(imageLoader: movie.#^COMPLETE^#

struct MoviesHomeGridMoviesRow_Previews {
}
