// RUN: %target-typecheck-verify-swift -solver-scope-threshold=75000
// REQUIRES: objc_interop

import SwiftUI

extension View {
    func f1<V: Equatable>(of: V, _: @escaping () async -> Void) -> some View { self }
    func f1<V: Equatable>(of: V, _: @escaping (V, V) async -> Void) -> some View { self }
}

struct BenchmarkView: View {
    /* @State */ private var x1: Bool = false
    /* @State */ private var x2: Bool = false
    /* @State */ private var x3: Bool = false
    /* @State */ private var x4: Bool = false
    /* @State */ private var x5: Bool = false

    /* @State */ private var x6: Double = 1.0
    /* @State */ private var x7: Double = 0.0
    /* @State */ private var x8: Double = 1.0
    /* @State */ private var x9: Double = 1.0
    /* @State */ private var x10: Double = 1.0

    /* @State */ private var x11: Int = 0
    /* @State */ private var x12: Int = 0
    /* @State */ private var x13: Bool = true
    /* @State */ private var x14: Bool = true
    /* @State */ private var x15: Bool = false
    /* @State */ private var x16: Bool = false

    /* @State */ private var x17: String? = nil
    /* @State */ private var x18: [Int]? = nil
    /* @State */ private var x19: String? = nil
    /* @State */ private var x20: [Int] = []
    /* @State */ private var x21: Int = 0
    /* @State */ private var x22: Int = 0
    /* @State */ private var x23: Bool = false
    /* @State */ private var x24: Int = 0

    var body: some View {
        Group {
            if x1 {
                Image(systemName: "")
                    .resizable()
                    .aspectRatio(contentMode: .fill)
                    .clipped()
            } else {
                Color.black
            }
        }
        .opacity(x6)
        .overlay {
            Text("")
                .opacity(x7)
        }
        .overlay {
            Color.clear
        }
        .overlay {
            LinearGradient(
                colors: [.clear, .black],
                startPoint: .top,
                endPoint: .bottom
            )
            .opacity(x10)
        }
        .overlay(alignment: .bottomTrailing) {
            if x11 == 0 {
                Text("")
                    .padding(.bottom, 0.0)
                    .opacity(x8)
            } else if x11 == 0 {
                HStack {
                    Text("")
                        .padding(.leading, 0.0)
                        .padding(.bottom, 0.0)
                    Spacer(minLength: 0)
                }
            }
        }
        .overlay {
            Color.clear
                .contentShape(Rectangle())
                .onTapGesture(count: 0) {
                    guard x13 else { return }
                    print("")
                }
        }
        .overlay {
            if x12 != 0 {
                VStack {
                    if x12 > 0 {
                        Spacer()
                    }
                    HStack {
                        Spacer()
                        Text("")
                            .padding(.top, 0)
                            .padding(.bottom, 0)
                    }
                    if x12 < 0 {
                        Spacer()
                    }
                }
                .padding(.top, 0)
                .padding(.trailing, 0)
            }
        }
        .overlay {
            HStack {
                Color.clear.frame(width: 0)
                VStack {
                    Color.clear.frame(height: 0)
                    Spacer()
                    Color.clear.frame(height: 0)
                }
                Color.clear.frame(width: 0)
            }
        }
        .overlay(alignment: .bottomTrailing) {
            ZStack {
                let str: String? = x11 == 2 ? "" : nil
                Text("\(str ?? "")")
                if x16 {
                    VStack {
                        Text("")
                        Text("")
                        Text("")
                    }
                    .font(.system(size: 0.0))
                    .opacity(x9)
                }
            }
            .foregroundStyle(.white)
            .padding(0)
        }
        .overlay(alignment: .topTrailing) {
            if x14 {
                Button("") {}
                    .opacity(x9)
                    .padding([.top, .leading, .trailing], 0.0)
            }
        }
        .f1(of: x17) {
            
            print("")
        }
        .f1(of: x18) { oldValue, newValue in
            guard oldValue?.count != newValue?.count else {
                return
            }
            print("")
        }
        .f1(of: x19) {
            if x11 == 0 {
                print("")
            }
        }
        .f1(of: x20) {
            print("")
        }
        .f1(of: x21) {
            print("")
        }
        .f1(of: x22) {
            print("")
        }
        .f1(of: [
            x2,
            x3,
            x4,
            x5
        ]) {
            print("")
        }
        .f1(of: x23) {
            print("")
            if !x23 {
                print("")
            }
        }
        .ignoresSafeArea()
        .onAppear {
            print("")
            if x19 == nil, x17 != nil {
                print("")
            }
            print("")
        }
        .f1(of: x24) {
            print("")
        }
        .background(x15 ? Color.black : Color.clear)
        .colorScheme(.light)
        .ignoresSafeArea()
        .accessibilityElement(children: .combine)
    }
}
