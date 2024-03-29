load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

load("@//:packages.bzl", "PACKAGES")

STACKAGE_ALL = ["@stackage//:{}".format(it) for it in PACKAGES]

haskell_binary(
    name = "num-inversions-hs",
    srcs = ["NumInversions/NumInversions.hs"],
    data = glob(["NumInversions/*.txt"]),
    deps = STACKAGE_ALL,
)

load("@rules_cc//cc:defs.bzl", "cc_binary")

cc_binary(
    name = "num-inversions-cpp",
    srcs = ["NumInversions/NumInversions.cpp"],
    data = glob(["NumInversions/*.txt"]),
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
    deps = STACKAGE_ALL,
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
    deps = STACKAGE_ALL,
)

cc_binary(
    name = "scc-cpp",
    srcs = ["SCC/SCC.cpp"],
    data = glob(["SCC/*.txt"]),
)

haskell_binary(
    name = "scc-hs",
    srcs = ["SCC/SCC.hs"],
    data = glob(["SCC/*.txt"]),
    deps = STACKAGE_ALL,
)

cc_binary(
    name = "shortest-path-cpp",
    srcs = ["ShortestPath/ShortestPath.cpp"],
    data = glob(["ShortestPath/*.txt"]),
)

haskell_binary(
    name = "shortest-path-hs",
    srcs = ["ShortestPath/ShortestPath.hs"],
    data = glob(["ShortestPath/*.txt"]),
    deps = STACKAGE_ALL,
)

cc_binary(
    name = "two-sum-cpp",
    srcs = ["TwoSum/TwoSum.cpp"],
    data = glob(["TwoSum/*.txt"]),
)

haskell_binary(
    name = "two-sum-hs",
    srcs = ["TwoSum/TwoSum.hs"],
    data = glob(["TwoSum/*.txt"]),
    deps = STACKAGE_ALL,
)

cc_binary(
    name = "median-stream-cpp",
    srcs = ["MedianStream/MedianStream.cpp"],
    data = glob(["MedianStream/*.txt"]),
)

haskell_binary(
    name = "median-stream-hs",
    srcs = ["MedianStream/MedianStream.hs"],
    data = glob(["MedianStream/*.txt"]),
    deps = STACKAGE_ALL,
)
