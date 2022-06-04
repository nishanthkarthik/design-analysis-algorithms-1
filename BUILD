load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

haskell_binary(
    name = "num-inversions-hs",
    srcs = ["NumInversions/NumInversions.hs"],
    deps = [
        "@stackage//:HUnit",
        "@stackage//:base",
        "@stackage//:vector",
    ],
    data = glob(["NumInversions/*.txt"]),
)

load("@rules_cc//cc:defs.bzl", "cc_binary")

cc_binary(
    name = "num-inversions-cpp",
    srcs = ["NumInversions/NumInversions.cpp"],
    data = ["NumInversions/tests.txt", "NumInversions/IntegerArraySpaced.txt"],
)

cc_binary(
    name = "quick-sort-cpp",
    srcs = ["QuickSort/QuickSort.cpp"],
    data = glob(["QuickSort/*.txt"]),
)

haskell_binary(
    name = "quick-sort-hs",
    srcs = ["QuickSort/QuickSort.hs"],
    data = glob(["QuickSort/*.txt"]),
    deps = ["@stackage//:vector", "@stackage//:base"],
)

cc_binary(
    name = "min-cut-cpp",
    srcs = ["MinCut/MinCut.cpp"],
    data = glob(["MinCut/*.txt"]),
)
