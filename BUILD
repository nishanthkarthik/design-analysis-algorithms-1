load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

haskell_binary(
    name = "num-inversions-hs",
    srcs = ["NumInversions/NumInversions.hs"],
    data = glob(["NumInversions/*.txt"]),
    deps = [
        "@stackage//:HUnit",
        "@stackage//:base",
        "@stackage//:vector",
    ],
)

load("@rules_cc//cc:defs.bzl", "cc_binary")

cc_binary(
    name = "num-inversions-cpp",
    srcs = ["NumInversions/NumInversions.cpp"],
    data = [
        "NumInversions/IntegerArraySpaced.txt",
        "NumInversions/tests.txt",
    ],
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
    deps = [
        "@stackage//:base",
        "@stackage//:vector",
    ],
)

cc_binary(
    name = "min-cut-cpp",
    srcs = ["MinCut/MinCut.cpp"],
    data = glob(["MinCut/*.txt"]),
)

haskell_binary(
    name = "min-cut-hs",
    srcs = ["MinCut/MinCut.hs"],
    data = glob(["MinCut/*.txt"]),
    deps = [
        "@stackage//:base",
        "@stackage//:containers",
        "@stackage//:random",
    ],
)

cc_binary(
    name = "scc-cpp",
    srcs = ["SCC/SCC.cpp"],
    data = glob(["SCC/*.txt"]),
)
